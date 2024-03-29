/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.faces.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, Closeable, File}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.Calendar
import java.util.Map.Entry
import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{DiscreteDomain, PointId, UnstructuredPointsDomain, Vectorizer}
import scalismo.color.RGB
import scalismo.faces.momo.{MoMo, MoMoBasic, MoMoExpress, PancakeDLRGP}
import scalismo.faces.utils.ResourceManagement
import scalismo.geometry.{_3D, EuclideanVector, Landmark, Point}
import scalismo.io.statisticalmodel.{HDF5Reader, HDF5Writer, NDArray, StatisticalModelIOUtils, StatisticalModelReader}
import io.jhdf.HdfFile
import scalismo.hdf5json.HDFPath
import scalismo.hdf5json.internal.HDFFile
import scalismo.io.LandmarkIO
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D}
import scalismo.statisticalmodel.ModelHelpers

import scala.io.Source
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
 * IO for Morphable Models
 */
object MoMoIO {

  case class WrappedStatisticalModelReader(reader: StatisticalModelReader) extends Closeable {
    def close() = reader.close()
  }

  /** cache to hold open models, identified by their URI */
  private val cacheSizeHint = 10
  private val openMoMos = new java.util.LinkedHashMap[URI, MoMo](cacheSizeHint, 0.75f, false) {
    override def removeEldestEntry(eldest: Entry[URI, MoMo]): Boolean = size() > cacheSizeHint
  }

  /**
   * clears all cached models
   */
  def clearURICache(): Unit = openMoMos.synchronized { openMoMos.clear() }

  /**
   * load a Morphable Model from an URI (cached)
   *
   * @param uri
   *   URI of model to open, must be a file currently, use "#" to indicate the path inside the file
   * @return
   */
  def read(uri: URI): Try[MoMo] = Try {
    if (uri.getScheme != "file") throw new RuntimeException("Only supports loading from file:// URI")

    // separate model file and model path inside file
    val modelURI = new URI(uri.getScheme + ":" + uri.getSchemeSpecificPart)
    val modelPath = Option(uri.getFragment).getOrElse("/")

    // map access
    var cachedModel: Option[MoMo] = None

    // look in cache
    openMoMos.synchronized {
      cachedModel = Option(openMoMos.get(uri))
    }

    // open if necessary
    cachedModel.getOrElse {
      val model = read(new File(modelURI), modelPath).get
      openMoMos.synchronized {
        openMoMos.put(uri, model)
      }
      model
    }
  }

  /**
   * load a Morphable Model from a file
   *
   * @param file
   *   file to open
   * @param modelPath
   *   path of the model inside file
   * @return
   */
  def read(file: File, modelPath: String = "/"): Try[MoMo] = {
    // open HDF5 at path
    ResourceManagement.usingTry(
      StatisticalModelIOUtils.openFileForReading(file).map(t => WrappedStatisticalModelReader(t))
    ) { (wrapper: WrappedStatisticalModelReader) =>
      readFromHDF5(wrapper.reader, HDFPath(modelPath))
    }
  }

  /**
   * load a Morphable Model from an open HDF5 file
   *
   * @param h5file
   *   HDF5 file
   * @param modelPath
   *   path of the model inside file
   * @return
   */
  def readFromHDF5(h5file: StatisticalModelReader, modelPath: HDFPath): Try[MoMo] = Try {
    val path = MoMoPathBuilder(modelPath)

    val shapeMesh = readGravisModelRepresenter(h5file, path.shape).get
    val shapeModel = readStatisticalModel3D[Point[_3D]](h5file, path.shape, shapeMesh).get

    val colorMesh = readGravisModelRepresenter(h5file, path.color).get
    val colorModel = readStatisticalModel3D[RGB](h5file, path.color, colorMesh).get

    // check shape, color and expression compatibility
    if (shapeMesh.pointSet != colorMesh.pointSet)
      throw new Exception("shape and model do not share a domain, different underlying point sets")

    // extract landmarks
    val landmarks = readLandmarks(h5file, path.landmarks).getOrElse(Map.empty[String, Landmark[_3D]])

    // expression model defaults to empty model
    if (h5file.exists(path.expression)) {
      val expressionMesh = readGravisModelRepresenter(h5file, path.expression).get
      val expressionModel = readStatisticalModel3D[EuclideanVector[_3D]](h5file, path.expression, expressionMesh).get
      if (shapeMesh.pointSet != expressionMesh.pointSet)
        throw new Exception("expression model does not share a domain, different underlying point sets")
      MoMo(shapeMesh, shapeModel, colorModel, expressionModel, landmarks)
    } else
      MoMo(shapeMesh, shapeModel, colorModel, landmarks)
  }

  /**
   * write a Morphable Model to a file, creates a HDF5 file
   *
   * @param file
   *   File object pointing to new file, will be created
   * @param modelPath
   *   path inside HDF5 file to put the model
   */
  def write(model: MoMo, file: File, modelPath: String = "/"): Try[Unit] = Try {
    ResourceManagement.using(StatisticalModelIOUtils.createFile(file).get) { (h5file: HDF5Writer) =>
      writeToHDF5(model, h5file, HDFPath(modelPath)).get
    }
  }

  /**
   * write a Morphable Model into an open HDF5 file
   *
   * @param h5file
   *   File object pointing to open HDF5 file
   * @param modelPath
   *   path inside HDF5 file to put model (leave empty for default)
   */
  def writeToHDF5(momo: MoMo, h5file: HDF5Writer, modelPath: HDFPath): Try[Unit] = Try {
    val path = MoMoPathBuilder(modelPath)

    // version information, 0.9 is our C++ standard format
    h5file.createGroup(path.version).get
    h5file.writeInt(path.majorVersion, 0).get
    h5file.writeInt(path.minorVersion, 9).get

    // catalog entries
    writeCatalog(momo, h5file, path).get

    // write model
    momo match {
      case momoExpress: MoMoExpress => writeMoMoExpress(momoExpress, h5file, path).get
      case momo: MoMoBasic          => writeMoMoBasic(momo, h5file, path).get
      case modelType => throw new IllegalArgumentException("cannot write model of type: " + modelType.getClass.getName)
    }

    // write meta data
    h5file.createGroup(path.metadata).get

    // write landmarks
    val landmarksPath = path.landmarks
    h5file.createGroup(landmarksPath).get
    writeLandmarks(momo.landmarks, h5file, landmarksPath).get
  }

  // -- private detail writers / readers --

  private def writeCatalog(momo: MoMo, h5file: HDF5Writer, path: MoMoPathBuilder): Try[Unit] = Try {
    h5file.createGroup(path.catalog).get
    // Morphable Model entry
    val catalogMorphableModelPath = path.catalog / "MorphableModel"
    h5file.createGroup(catalogMorphableModelPath).get
    h5file.writeString(catalogMorphableModelPath / "modelType", "CUSTOM_MODEL").get
    h5file.writeString(catalogMorphableModelPath / "modelPath", "/").get

    // Shape model entry
    val catalogShapePath = path.catalog / "MorphableModel.shape"
    h5file.createGroup(catalogShapePath).get
    h5file.writeString(catalogShapePath / "modelType", "POLYGON_MESH_MODEL").get
    h5file.writeString(catalogShapePath / "modelPath", path.shape.toString).get

    // Color model entry
    val catalogColorPath = path.catalog / "MorphableModel.color"
    h5file.createGroup(catalogColorPath).get
    h5file.writeString(catalogColorPath / "modelType", "POLYGON_MESH_DATA_MODEL").get
    h5file.writeString(catalogColorPath / "modelPath", path.color.toString).get

    if (momo.hasExpressions) {
      // catalog: expression model entry
      val catalogExpressionPath = path.catalog / "MorphableModel.expression"
      h5file.createGroup(catalogExpressionPath).get
      h5file.writeString(catalogExpressionPath / "modelType", "POLYGON_MESH_DATA_MODEL").get
      h5file.writeString(catalogExpressionPath / "modelPath", path.expression.toString).get
    }
  }

  /** write a Morphable Model with expression into an opened HDF5 file */
  private def writeMoMoExpress(momo: MoMoExpress, h5file: HDF5Writer, path: MoMoPathBuilder): Try[Unit] = Try {
    // write shape model
    val shapePath = path.shape
    h5file.createGroup(shapePath).get
    writeStatisticalModel(momo.shape, h5file, shapePath).get
    writeShapeRepresenter(momo.referenceMesh, h5file, shapePath).get

    // write color model
    val colorPath = path.color
    h5file.createGroup(colorPath).get
    writeStatisticalModel(momo.color, h5file, colorPath).get
    writeColorRepresenter(momo.referenceMesh, h5file, colorPath).get

    // expression model
    val expressionPath = path.expression
    h5file.createGroup(expressionPath).get
    writeStatisticalModel(momo.expression, h5file, expressionPath).get
    writeExpressionRepresenter(momo.referenceMesh, h5file, expressionPath).get
  }

  /** write a Morphable Model with expression into an opened HDF5 file */
  private def writeMoMoBasic(momo: MoMoBasic, h5file: HDF5Writer, path: MoMoPathBuilder): Try[Unit] = Try {
    // write shape model
    val shapePath = path.shape
    h5file.createGroup(shapePath).get
    writeStatisticalModel(momo.shape, h5file, shapePath).get
    writeShapeRepresenter(momo.referenceMesh, h5file, shapePath).get

    // write color model
    val colorPath = path.color
    h5file.createGroup(colorPath).get
    writeStatisticalModel(momo.color, h5file, colorPath).get
    writeColorRepresenter(momo.referenceMesh, h5file, colorPath).get
  }

  private def readGravisModelRepresenter(h5file: StatisticalModelReader, modelPath: HDFPath): Try[TriangleMesh3D] =
    Try {
      val representerPath = representerPathBuilder(modelPath)

      val representerName = h5file.readStringAttribute(representerPath, "name").get
      // read mesh according to type given in representer
      val mesh: Try[TriangleMesh3D] = representerName match {
        case "gravis::MeshShapeRepresenter"      => readPolygonRepresenterMesh(h5file, modelPath)
        case "gravis::MeshColorRepresenter"      => readPolygonRepresenterMesh(h5file, modelPath)
        case "gravis::MeshExpressionRepresenter" => readPolygonRepresenterMesh(h5file, modelPath)
        case "gravis::Mesh Shape Representer"    => readLegacyRepresenterMesh(h5file, modelPath)
        case ("gravis::Mesh Color Representer" | " gravis::Mesh Color Representer") =>
          readLegacyRepresenterMesh(h5file, modelPath)
        case _ =>
          h5file.readStringAttribute(representerPath, "datasetType") match {
            case Success("POLYGON_MESH")      => readPolygonRepresenterMesh(h5file, modelPath)
            case Success("POLYGON_MESH_DATA") => readPolygonRepresenterMesh(h5file, modelPath)
            case Success(datasetType) =>
              Failure(
                new Exception(
                  s"can only read model of datasetType POLYGON_MESH or POLYGON_MESH_DATA. Got $datasetType instead"
                )
              )
            case Failure(exc) => Failure(new RuntimeException("unknown representer format (no datasetType attribute)"))
          }
      }
      mesh.get
    }

  private def readLegacyRepresenterMesh(h5file: StatisticalModelReader, modelPath: HDFPath): Try[TriangleMesh3D] = Try {
    val representerPath = representerPathBuilder(modelPath)

    val vertArray = h5file.readNDArrayFloat(representerPath / "reference-mesh" / "vertex-coordinates").get
    if (vertArray.dims(1) != 3)
      throw new Exception("the representer points are not 3D points")

    val vertMat = ndFloatArrayToMatrix(vertArray)
    val points = for (i <- 0 until vertMat.rows) yield Point(vertMat(i, 0), vertMat(i, 1), vertMat(i, 2))

    val cellArray = h5file.readNDArrayInt(representerPath / "reference-mesh" / "triangle-list").get
    if (cellArray.dims(1) != 3)
      throw new Exception("the representer cells are not triangles")

    val cellMat = ndIntArrayToMatrix(cellArray)
    val cells =
      for (i <- 0 until cellMat.rows)
        yield TriangleCell(PointId(cellMat(i, 0)), PointId(cellMat(i, 1)), PointId(cellMat(i, 2)))

    TriangleMesh3D(points, TriangleList(cells))
  }

  private def readPolygonRepresenterMesh(h5file: StatisticalModelReader, modelPath: HDFPath): Try[TriangleMesh3D] =
    Try {
      val representerPath = representerPathBuilder(modelPath)

      val vertArray = h5file.readNDArrayFloat(representerPath / "points").get
      if (vertArray.dims(0) != 3)
        throw new Exception("the representer points are not 3D points")

      val vertMat = ndFloatArrayToMatrix(vertArray)
      val points = for (i <- 0 until vertMat.cols) yield Point(vertMat(0, i), vertMat(1, i), vertMat(2, i))

      val cellArray = Try { h5file.readNDArrayInt(representerPath / "cells").get }.getOrElse({
        throw IllegalArgumentException(
          "Probably you tried to read a legacy model file. Please convert the datatype of the cells data to the type int."
        )
      })
      if (cellArray.dims(0) != 3)
        throw new Exception("the representer cells are not triangles")

      val cellMat = ndIntArrayToMatrix(cellArray)
      val cells =
        for (i <- 0 until cellMat.cols)
          yield TriangleCell(PointId(cellMat(0, i)), PointId(cellMat(1, i)), PointId(cellMat(2, i)))

      TriangleMesh3D(points, TriangleList(cells))
    }

  /** read a statistical statismo model from a HDF5 file at path modelPath */
  private def readStatisticalModel3D[A](h5file: StatisticalModelReader,
                                        modelPath: HDFPath,
                                        reference: TriangleMesh[_3D]
  )(implicit vectorizer: Vectorizer[A]): Try[PancakeDLRGP[_3D, TriangleMesh, A]] = {
    val path = StatisticalModelPathBuilder(modelPath)

    for {
      meanArray <- h5file.readArrayFloat(path.mean)
      meanVector = DenseVector(meanArray.map { _.toDouble })
      pcaBasisArray <- h5file.readNDArrayFloat(path.pcaBasis)
      majorVersion <- h5file.readInt(path.majorVersion).recover { case (e: Exception) => 0 }
      minorVersion <- h5file.readInt(path.minorVersion).recover { case (e: Exception) => 8 }
      pcaVarianceArray <- h5file.readArrayFloat(path.pcaVariance)
      pcaVarianceVector = DenseVector(pcaVarianceArray.map { _.toDouble })
      noiseVarianceArray <- h5file.readArrayFloat(path.noiseVariance)
      noiseVariance = noiseVarianceArray(0).toDouble
      pcaBasisMatrix = ndFloatArrayToMatrix(pcaBasisArray).map { _.toDouble }
      pcaBasis <- (majorVersion, minorVersion) match {
        case (1, _) => Success(pcaBasisMatrix)
        case (0, 9) => Success(pcaBasisMatrix)
        case (0, 8) =>
          Success(extractOrthonormalPCABasisMatrix(pcaBasisMatrix, pcaVarianceVector)) // an old statismo version
        case v => Failure(new RuntimeException(s"Unsupported version ${v._1}.${v._2}"))
      }
    } yield {
      PancakeDLRGP(ModelHelpers.buildFrom(reference, meanVector, pcaVarianceVector, pcaBasis), noiseVariance)
    }
  }

  private def readLandmarks(h5file: StatisticalModelReader, path: HDFPath): Try[Map[String, Landmark[_3D]]] = Try {
    if (h5file.exists(path / "json")) {
      // json reader, modern format
      val jsonString = h5file.readString(path / "json").get
      val lmList = LandmarkIO.readLandmarksJsonFromSource[_3D](Source.fromString(jsonString)).get
      lmList.map(lm => lm.id -> lm).toMap
    } else if (h5file.exists(path / "text")) {
      // legacy tlms text format
      val tlmsString = h5file.readString(path / "text").get
      val lmList =
        TLMSLandmarksIO.read3DFromStream(new ByteArrayInputStream(tlmsString.getBytes(StandardCharsets.UTF_8))).get
      lmList.map(lm => lm.id -> Landmark[_3D](lm.id, lm.point, None, None)).toMap
    } else
      throw new Exception("No landmarks present")
  }

  private def writeShapeRepresenter(mesh: TriangleMesh3D, h5file: HDF5Writer, modelPath: HDFPath): Try[Unit] = Try {
    val representerPath = representerPathBuilder(modelPath)
    h5file.createGroup(representerPath).get
    h5file.writeStringAttribute(representerPath, "name", "gravis::MeshShapeRepresenter").get
    h5file.writeStringAttribute(representerPath, "datasetType", "POLYGON_MESH").get
    writeTriangleMesh3D(mesh, h5file, representerPath).get
  }

  private def writeColorRepresenter(mesh: TriangleMesh3D, h5file: HDF5Writer, modelPath: HDFPath): Try[Unit] = Try {
    val representerPath = representerPathBuilder(modelPath)
    h5file.createGroup(representerPath).get
    h5file.writeStringAttribute(representerPath, "name", "gravis::MeshColorRepresenter").get
    h5file.writeStringAttribute(representerPath, "datasetType", "POLYGON_MESH").get
    h5file
      .writeString(representerPath / "colorspace", "RGB" + '\u0000')
      .get // ugly 0-termination string hack because statismo in C++ stores strings including 0 termination
    writeTriangleMesh3D(mesh, h5file, representerPath).get
  }

  private def writeExpressionRepresenter(mesh: TriangleMesh3D, h5file: HDF5Writer, modelPath: HDFPath): Try[Unit] =
    Try {
      val representerPath = representerPathBuilder(modelPath)
      h5file.createGroup(representerPath).get
      h5file.writeStringAttribute(representerPath, "name", "gravis::MeshExpressionRepresenter").get
      h5file.writeStringAttribute(representerPath, "datasetType", "POLYGON_MESH").get
      writeTriangleMesh3D(mesh, h5file, representerPath).get
    }

  private def writeTriangleMesh3D(mesh: TriangleMesh3D, h5file: HDF5Writer, path: HDFPath): Try[Unit] = Try {
    h5file.createGroup(path).get
    val points = mesh.pointSet.points.toIndexedSeq
    val cells = mesh.cells
    val pointData: IndexedSeq[Double] = points.map(_.x) ++ points.map(_.y) ++ points.map(_.z)
    h5file.writeNDArray(path / "points", NDArray(IndexedSeq(3, points.size), pointData.toArray.map(_.toFloat))).get
    val cellData: IndexedSeq[PointId] = cells.map(_.ptId1) ++ cells.map(_.ptId2) ++ cells.map(_.ptId3)
    h5file.writeNDArray(path / "cells", NDArray(IndexedSeq(3, cells.size), cellData.toArray.map(_.id))).get
  }

  private def writeStatisticalModel[A](model: PancakeDLRGP[_3D, TriangleMesh, A],
                                       h5file: HDF5Writer,
                                       modelPath: HDFPath
  ): Try[Unit] = Try {
    val path = StatisticalModelPathBuilder(modelPath)
    val mean = model.meanVector.map(_.toFloat)
    val variance = model.variance.map(_.toFloat)
    val pcaBasis = model.basisMatrix.map(_.toFloat)

    h5file.createGroup(path.version).get
    h5file.writeInt(path.majorVersion, 0).get
    h5file.writeInt(path.minorVersion, 9).get

    h5file.writeArray[Float](path.mean, mean.toArray).get
    h5file.writeArray[Float](path.noiseVariance, Array(model.noiseVariance.toFloat)).get
    h5file
      .writeNDArray[Float](path.pcaBasis,
                           NDArray(IndexedSeq(pcaBasis.rows, pcaBasis.cols), pcaBasis.t.flatten(false).toArray)
      )
      .get
    h5file.writeArray[Float](path.pcaVariance, variance.toArray).get
    h5file.writeString(path.buildTime, Calendar.getInstance.getTime.toString).get
    h5file.writeNDArray[Float](path.scores, NDArray(IndexedSeq(1, 1), Array(0.0f))).get
  }

  private def writeLandmarks(landmarks: Map[String, Landmark[_3D]], h5file: HDF5Writer, path: HDFPath): Try[Unit] =
    Try {
      val jsonStream = new ByteArrayOutputStream()
      LandmarkIO.writeLandmarksJsonToStream(landmarks.values.toList, jsonStream)
      h5file.writeString(path / "json", jsonStream.toString("UTF-8"))
    }

  /*
   Path builder to ensure we have the same paths when reading and writing
   */
  private case class MoMoPathBuilder(modelPath: HDFPath) {
    def catalog: HDFPath = modelPath / "catalog"

    def version: HDFPath = modelPath / "version"

    def majorVersion: HDFPath = version / "majorVersion"

    def minorVersion: HDFPath = version / "minorVersion"

    def shape: HDFPath = modelPath / "shape"

    def color: HDFPath = modelPath / "color"

    def expression: HDFPath = modelPath / "expression"

    def metadata: HDFPath = modelPath / "metadata"

    def landmarks: HDFPath = metadata / "landmarks"
  }

  private case class StatisticalModelPathBuilder(modelPath: HDFPath) {

    def version: HDFPath = modelPath / "version"

    def majorVersion: HDFPath = version / "majorVersion"

    def minorVersion: HDFPath = version / "minorVersion"

    def model: HDFPath = modelPath / "model"

    def mean: HDFPath = model / "mean"

    def noiseVariance: HDFPath = model / "noiseVariance"

    def pcaBasis: HDFPath = model / "pcaBasis"

    def pcaVariance: HDFPath = model / "pcaVariance"

    def modelinfo: HDFPath = modelPath / "modelinfo"

    def buildTime: HDFPath = modelinfo / "build-time"

    def scores: HDFPath = modelinfo / "scores"
  }

  private def representerPathBuilder(modelPath: HDFPath) = modelPath / "representer"

  /*
   Helpers to handle matrices and arrays.
   */
  private def ndFloatArrayToMatrix(array: NDArray[Float]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def ndDoubleArrayToMatrix(array: NDArray[Double]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def ndIntArrayToMatrix(array: NDArray[Int]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  /*
   Helper function to handle old statismo format.
   */
  private def extractOrthonormalPCABasisMatrix(pcaBasisMatrix: DenseMatrix[Double],
                                               pcaVarianceVector: DenseVector[Double]
  ): DenseMatrix[Double] = {
    // this is an old statismo format, that has the pcaVariance directly stored in the PCA matrix,
    // i.e. pcaBasis = U * sqrt(lmbda), where U is a matrix of eigenvectors and lmbda the corresponding eigenvalues.
    // We recover U from it by normalizing all eigenvectors
    val U = DenseMatrix.zeros[Double](pcaBasisMatrix.rows, pcaBasisMatrix.cols)
    for (i <- 0 until pcaBasisMatrix.cols) {
      val pcVec: DenseVector[Double] = pcaBasisMatrix(::, i).toDenseVector
      U(::, i) := breeze.linalg.normalize(pcVec)
    }
    U
  }

}
