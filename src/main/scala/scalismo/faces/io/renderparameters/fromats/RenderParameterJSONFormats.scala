package scalismo.faces.io.renderparameters.fromats

import scalismo.color.{ColorSpaceOperations, RGB, RGBA}
import scalismo.common.PointId
import scalismo.faces.image.{AccessMode, ColumnMajorImageDomain, PixelImage, RowMajorImageDomain}
import scalismo.faces.mesh.{ColorNormalMesh3D, IndirectProperty, TextureMappedProperty, VertexPropertyPerTriangle}
import scalismo.faces.parameters.*
import scalismo.geometry.*
import scalismo.numerics.ValueInterpolator
import scalismo.mesh.{
  MeshSurfaceProperty,
  SurfacePointProperty,
  TriangleCell,
  TriangleList,
  TriangleMesh3D,
  TriangleProperty,
  VertexColorMesh3D
}
import upickle.default
import upickle.default.{read, readwriter, writeJs as write, ReadWriter}

import java.net.URI
import scala.reflect.ClassTag
import scala.util.Try

object RenderParameterJSONFormats {

  implicit val uriMapper: upickle.default.ReadWriter[URI] = upickle.default
    .readwriter[String]
    .bimap[URI](
      uri => uri.toString,
      s => URI(s)
    )

  implicit val ev1DMapper: default.ReadWriter[EuclideanVector[_1D]] =
    readwriter[List[Double]].bimap[EuclideanVector[_1D]](
      ev => ev.toArray.toList,
      l => EuclideanVector(l.toArray)
    )

  implicit val ev2DMapper: default.ReadWriter[EuclideanVector[_2D]] =
    readwriter[List[Double]].bimap[EuclideanVector[_2D]](
      ev => ev.toArray.toList,
      l => EuclideanVector(l.toArray)
    )

  implicit val ev3DMapper: default.ReadWriter[EuclideanVector[_3D]] =
    readwriter[List[Double]].bimap[EuclideanVector[_3D]](
      ev => ev.toArray.toList,
      l => EuclideanVector(l.toArray)
    )

  implicit val pt1DMapper: default.ReadWriter[Point[_1D]] = readwriter[List[Double]].bimap[Point[_1D]](
    ev => ev.toArray.toList,
    l => Point(l.toArray)
  )

  implicit val pt2DMapper: default.ReadWriter[Point[_2D]] = readwriter[List[Double]].bimap[Point[_2D]](
    ev => ev.toArray.toList,
    l => Point(l.toArray)
  )

  implicit val pt3DMapper: default.ReadWriter[Point[_3D]] = readwriter[List[Double]].bimap[Point[_3D]](
    ev => ev.toArray.toList,
    l => Point(l.toArray)
  )

  implicit val rgbaMapper: default.ReadWriter[RGBA] = readwriter[List[Double]].bimap[RGBA](
    col => List(col.r, col.g, col.b, col.a),
    l => RGBA(l(0), l(1), l(2), l(3))
  )

  implicit val rgbMapper: default.ReadWriter[RGB] = readwriter[List[Double]].bimap[RGB](
    col => List(col.r, col.g, col.b),
    l => RGB(l(0), l(1), l(2))
  )

  implicit val shlMapper: default.ReadWriter[SphericalHarmonicsLight] =
    readwriter[SphericalHarmonicsLightSerializationFormat].bimap[SphericalHarmonicsLight](
      light => SphericalHarmonicsLightSerializationFormat(light.coefficients.map(_.toArray.toList).toList),
      ll => SphericalHarmonicsLight(ll.coefficients.map(l => EuclideanVector[_3D](l.toArray)).toIndexedSeq)
    )

  implicit val directionalLightMapper: default.ReadWriter[DirectionalLight] =
    readwriter[DirectLightSerializationFormat].bimap[DirectionalLight](
      dl => DirectLightSerializationFormat(dl),
      dlm => dlm.toDirectionalLight()
    )

  implicit val poseMapper: upickle.default.ReadWriter[Pose] = upickle.default.macroRW

  implicit val viewMapper: upickle.default.ReadWriter[ViewParameter] = upickle.default.macroRW

  implicit val cameraMapper: upickle.default.ReadWriter[Camera] = upickle.default.macroRW

  implicit val imageSizeMapper: upickle.default.ReadWriter[ImageSize] = upickle.default.macroRW

  implicit val colorTransformMapper: upickle.default.ReadWriter[ColorTransform] = upickle.default.macroRW

  implicit val triangleCellMapper: default.ReadWriter[TriangleCell] = readwriter[List[Double]].bimap[TriangleCell](
    cell => cell.pointIds.map(_.id.toDouble).toList,
    l =>
      TriangleCell(
        PointId(l(0).toInt),
        PointId(l(1).toInt),
        PointId(l(2).toInt)
      )
  )

  implicit val meshFileFormat: ReadWriter[MeshFile] = readwriter[ujson.Value].bimap[MeshFile](
    meshFile =>
      ujson.Obj(
        ("meshURI", write(meshFile.meshURI)),
        ("@type", "MeshFile")
      ),
    json => {
      MeshFile(
        meshURI = read[URI](json("meshURI"))
      )
    }
  )

  implicit val momoInstanceMapper: default.ReadWriter[MoMoInstance] =
    readwriter[MoMoInstanceSerializationFormat].bimap[MoMoInstance](
      mi => MoMoInstanceSerializationFormat(mi),
      mis => mis.toMoMo()
    )

  implicit val colSpaceOps2D: ColorSpaceOperations[Point[_2D]] = new ColorSpaceOperations[Point[_2D]] {
    override def add(pix1: Point[_2D], pix2: Point[_2D]): Point[_2D] = (pix1.toVector + pix2.toVector).toPoint
    override def multiply(pix1: Point[_2D], pix2: Point[_2D]): Point[_2D] = Point(pix1.x * pix2.x, pix1.y * pix2.y)
    override def dot(pix1: Point[_2D], pix2: Point[_2D]): Double = pix1.toVector.dot(pix2.toVector)
    override def dimensionality: Int = 2
    override def scale(pix: Point[_2D], l: Double): Point[_2D] = (pix.toVector * l).toPoint
    override def zero: Point[_2D] = Point2D.origin
  }

  implicit def pixelImageFormat[A: ClassTag](implicit formatA: ReadWriter[A]): ReadWriter[PixelImage[A]] =
    readwriter[ujson.Value].bimap[PixelImage[A]](
      pi =>
        ujson.Obj(
          "width" -> pi.width,
          "height" -> pi.height,
          "domainMode" -> (pi.domain match {
            case _: ColumnMajorImageDomain => "ColumnMajor"
            case _: RowMajorImageDomain    => "RowMajor"
          }),
          "data" -> write(pi.values.toIndexedSeq)
        ),
      json => {
        val width = read[Int](json("width"))
        val height = read[Int](json("height"))
        val data = read[IndexedSeq[A]](json("data"))
        val domain = read[String](json("domainMode")) match {
          case "ColumnMajor" => ColumnMajorImageDomain(width, height)
          case "RowMajor"    => RowMajorImageDomain(width, height)
          case s: String     => throw new IllegalArgumentException(s"unknown image domain mode: $s")
        }

        PixelImage(domain, data).withAccessMode(AccessMode.Repeat())
      }
    )

  implicit def meshSurfaceProperty[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A],
    colorSpaceOperations: ColorSpaceOperations[A]
  ): ReadWriter[MeshSurfaceProperty[A]] = readwriter[ujson.Value].bimap[MeshSurfaceProperty[A]](
    obj =>
      obj match {
        case prop: SurfacePointProperty[A]      => write(prop)
        case prop: TriangleProperty[A]          => write(prop)
        case prop: IndirectProperty[A]          => write(prop)
        case prop: VertexPropertyPerTriangle[A] => write(prop)
        case prop: TextureMappedProperty[A]     => write(prop)
        case _ => throw new IllegalArgumentException("cannot serialize MeshSurfaceProperty, unknown type")
      },
    json =>
      json("@type").str match {
        case "SurfacePointProperty"      => read[SurfacePointProperty[A]](json)
        case "TriangleProperty"          => read[TriangleProperty[A]](json)
        case "IndirectProperty"          => read[IndirectProperty[A]](json)
        case "VertexPropertyPerTriangle" => read[VertexPropertyPerTriangle[A]](json)
        case "TextureMappedProperty"     => read[TextureMappedProperty[A]](json)
        case _                           => throw new IllegalArgumentException(s"Unknown type of MeshSurfaceProperty")
      }
  )

  implicit def surfacePointPropertyFormat[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A]
  ): default.ReadWriter[SurfacePointProperty[A]] = readwriter[ujson.Value].bimap[SurfacePointProperty[A]](
    spp => {
      ujson.Obj(
        "triangles" -> write(spp.triangulation.triangles),
        "pointData" -> write(spp.pointData),
        "@type" -> "SurfacePointProperty"
      )
    },
    json => {
      assert(json("@type").str == "SurfacePointProperty")

      val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))
      val triangulation = TriangleList(triangles)

      val pointData = read[IndexedSeq[A]](json("pointData"))

      SurfacePointProperty(
        triangulation = triangulation,
        pointData = pointData
      )
    }
  )
  implicit def trianglePropertyFormat[A: ClassTag](implicit formatA: ReadWriter[A]): ReadWriter[TriangleProperty[A]] =
    readwriter[ujson.Value].bimap[TriangleProperty[A]](
      obj => {
        ujson.Obj(
          "triangles" -> write(obj.triangulation.triangles),
          "triangleData" -> write(obj.triangleData),
          "@type" -> "TriangleProperty"
        )
      },
      json => {
        val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))
        val triangulation = TriangleList(triangles)

        val triangleData = read[IndexedSeq[A]](json("triangleData"))

        TriangleProperty(
          triangulation = triangulation,
          triangleData = triangleData
        )
      }
    )

  implicit def vertexPropertyPerTriangleFormat[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A]
  ): ReadWriter[VertexPropertyPerTriangle[A]] = readwriter[ujson.Value].bimap[VertexPropertyPerTriangle[A]](
    obj =>
      ujson.Obj(
        "triangles" -> write(obj.triangulation.triangles),
        "pointData" -> write(obj.vertexData),
        "triangleIndex" -> write(obj.triangleVertexIndex.map { v => (v.i, v.j, v.k) }),
        "@type" -> "VertexPropertyPerTriangle"
      ),
    json => {
      val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))
      val triangulation = TriangleList(triangles)

      val pointData = read[IndexedSeq[A]](json("pointData"))
      val triangleIndex = read[IndexedSeq[(Int, Int, Int)]](json("triangleIndex")).map { case (i, j, k) =>
        IntVector3D(i, j, k)
      }

      VertexPropertyPerTriangle(triangulation, triangleIndex, pointData)
    }
  )

  implicit def textureMappedPropertyFormat[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A],
    colorSpaceOperations: ColorSpaceOperations[A]
  ): ReadWriter[TextureMappedProperty[A]] = upickle.default
    .readwriter[ujson.Value]
    .bimap[TextureMappedProperty[A]](
      tmp =>
        ujson.Obj(
          "triangles" -> write(tmp.triangulation.triangles),
          "textureMapping" -> write(tmp.textureMapping),
          "texture" -> write(tmp.texture),
          "@type" -> "TextureMappedProperty"
        ),
      json => {
        assert(json("@type").str == "TextureMappedProperty")

        val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))
        val triangulation = TriangleList(triangles)

        val textureMapping = read[MeshSurfaceProperty[Point[_2D]]](json("textureMapping"))
        val texture = read[PixelImage[A]](json("texture"))

        TextureMappedProperty(triangulation, textureMapping, texture)
      }
    )

  implicit def indirectPropertyFormat[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A],
    colorSpaceOperations: ColorSpaceOperations[A]
  ): ReadWriter[IndirectProperty[A]] = readwriter[ujson.Value].bimap[IndirectProperty[A]](
    obj =>
      ujson.Obj(
        "triangles" -> write(obj.triangulation.triangles),
        "properties" -> write(obj.properties),
        "triangleIndirectionIndex" -> write(obj.triangleIndirectionIndex),
        "@type" -> "IndirectProperty"
      ),
    json => {
      val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))
      val triangulation = TriangleList(triangles)

      val properties = read[IndexedSeq[MeshSurfaceProperty[A]]](json("properties"))
      val triangleIndirectionIndex = read[IndexedSeq[Int]](json("triangleIndirectionIndex"))

      IndirectProperty(triangulation, triangleIndirectionIndex, properties)
    }
  )

  implicit val colorNormalMeshMapper: ReadWriter[ColorNormalMesh3D] = readwriter[ujson.Value].bimap[ColorNormalMesh3D](
    mesh =>
      ujson.Obj(
        "points" -> write(mesh.shape.pointSet.points.map(_.toVector).toIndexedSeq),
        "color" -> write(mesh.color),
        "normals" -> write(mesh.normals),
        "triangles" -> write(mesh.shape.triangles),
        "@type" -> "ColorNormalMesh"
      ),
    json => {
      val points = read[IndexedSeq[EuclideanVector[_3D]]](json("points")).map(_.toPoint)
      val color = read[MeshSurfaceProperty[RGBA]](json("color"))
      val normals = read[MeshSurfaceProperty[EuclideanVector[_3D]]](json("normals"))
      val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))

      val triangulation = TriangleList(triangles)
      val shape = TriangleMesh3D(points, triangulation)

      ColorNormalMesh3D(shape, color, normals)
    }
  )

  implicit val vertexColorMeshMapper: ReadWriter[VertexColorMesh3D] = readwriter[ujson.Value].bimap[VertexColorMesh3D](
    mesh =>
      ujson.Obj(
        "points" -> write(mesh.shape.pointSet.points.map(_.toVector).toIndexedSeq),
        "color" -> write(mesh.color.pointData),
        "triangles" -> write(mesh.shape.triangles),
        "@type" -> "VertexColorMesh"
      ),
    json => {
      val points = read[IndexedSeq[EuclideanVector[_3D]]](json("points")).map(_.toPoint)
      val color = read[IndexedSeq[RGBA]](json("color"))
      val triangles = read[IndexedSeq[TriangleCell]](json("triangles"))

      val triangulation = TriangleList(triangles)
      val shape = TriangleMesh3D(points, triangulation)

      require(color.length == points.length)

      VertexColorMesh3D(shape, SurfacePointProperty(triangulation, color))
    }
  )

  implicit val meshColorNormalsFormat: ReadWriter[MeshColorNormals] = readwriter[ujson.Value].bimap[MeshColorNormals](
    mesh => write(mesh.colorNormalMesh),
    json => MeshColorNormals(read[ColorNormalMesh3D](json))
  )

  implicit val meshVertexColorFormat: ReadWriter[MeshVertexColor] = readwriter[ujson.Value].bimap[MeshVertexColor](
    mesh => write(mesh.vertexColorMesh3D),
    json => MeshVertexColor(read[VertexColorMesh3D](json))
  )
}
