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

package scalismo.faces.io.renderparameters

import java.net.URI

import scalismo.common.PointId
import scalismo.color.{ColorSpaceOperations, RGB, RGBA}
import scalismo.faces.image._
import scalismo.faces.mesh._
import scalismo.faces.parameters._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.numerics.ValueInterpolator
import spray.json._

import scala.reflect.ClassTag

/** JSON object format with variable renderables and illumination */
trait RenderParameterJSONFormatV2 extends RenderParameterJSONFormatLegacy {
  override val version = "V2.0"

  // momo express writer and reader
  protected case class MoMoInstanceV2(shape: IndexedSeq[Double],
                                    color: IndexedSeq[Double],
                                    modelURI: URI) {
    def toMoMoInstance = MoMoInstance(shape, color, IndexedSeq.empty, modelURI)
  }

  // momo writer and reader
  protected implicit val momoInstanceV2Format: RootJsonFormat[MoMoInstanceV2] = new RootJsonFormat[MoMoInstanceV2] {
    def write(momo: MoMoInstanceV2): JsValue = JsObject(
      ("shape", momo.shape.toJson),
      ("color", momo.color.toJson),
      ("modelURI", momo.modelURI.toJson),
      ("@type", "MoMoInstance".toJson))

    def read(json: JsValue): MoMoInstanceV2 = {
      val fields = json.asJsObject(s"expected MoMoInstance object, got: $json").fields
      MoMoInstanceV2(
        shape = fields("shape").convertTo[IndexedSeq[Double]],
        color = fields("color").convertTo[IndexedSeq[Double]],
        modelURI = fields("modelURI").convertTo[URI])
    }
  }

  // momo instance writer and reader: maps to a MoMoExpressInstance in this format
  implicit val momoInstanceFormat: RootJsonFormat[MoMoInstance] = new RootJsonFormat[MoMoInstance] {
    def write(momo: MoMoInstance): JsValue = momoExpressInstanceV2Format.write(MoMoExpressInstanceV2(momo))
    def read(json: JsValue): MoMoInstance = json.convertTo[MoMoExpressInstanceV2].toMoMoInstance
  }

  // momo express writer and reader
  protected case class MoMoExpressInstanceV2(shape: IndexedSeq[Double],
                                           color: IndexedSeq[Double],
                                           expression: IndexedSeq[Double],
                                           modelURI: URI) {
    def toMoMoInstance = MoMoInstance(shape, color, expression, modelURI)
  }

  protected object MoMoExpressInstanceV2 {
    def apply(momo: MoMoInstance): MoMoExpressInstanceV2 = MoMoExpressInstanceV2(momo.shape, momo.color, momo.expression, momo.modelURI)
  }

  protected implicit val momoExpressInstanceV2Format: RootJsonFormat[MoMoExpressInstanceV2] = new RootJsonFormat[MoMoExpressInstanceV2] {
    def write(momo: MoMoExpressInstanceV2): JsValue = JsObject(
      ("shape", momo.shape.toJson),
      ("color", momo.color.toJson),
      ("expression", momo.expression.toJson),
      ("modelURI", momo.modelURI.toJson),
      ("@type", "MoMoExpressInstance".toJson))

    override def read(json: JsValue): MoMoExpressInstanceV2 = {
      val fields = json.asJsObject(s"expected MoMoInstance object, got: $json").fields
      MoMoExpressInstanceV2(
        shape = fields("shape").convertTo[IndexedSeq[Double]],
        color = fields("color").convertTo[IndexedSeq[Double]],
        expression = fields("expression").convertTo[IndexedSeq[Double]],
        modelURI = fields("modelURI").convertTo[URI])
    }
  }

  // mesh file
  implicit val meshFileFormat: RootJsonFormat[MeshFile] = new RootJsonFormat[MeshFile] {
    override def write(meshFile: MeshFile): JsValue = JsObject(
      ("meshURI", meshFile.meshURI.toJson),
      ("@type", "MeshFile".toJson)
    )

    override def read(json: JsValue): MeshFile = {
      val fields = json.asJsObject(s"expected MeshFile object, got: $json").fields
      MeshFile(
        meshURI = fields("meshURI").convertTo[URI]
      )
    }
  }

  implicit val triangleCellFormat: JsonFormat[TriangleCell] = new JsonFormat[TriangleCell] {
    override def read(json: JsValue): TriangleCell = json match {
      case JsArray(Seq(JsNumber(p1), JsNumber(p2), JsNumber(p3))) => TriangleCell(PointId(p1.toInt), PointId(p2.toInt), PointId(p3.toInt))
      case _ => throw new DeserializationException(s"expected TriangleCell, got $json")
    }

    override def write(obj: TriangleCell): JsValue = JsArray(obj.ptId1.id.toJson, obj.ptId2.id.toJson, obj.ptId3.id.toJson)
  }

  implicit def surfacePointPropertyFormat[A](implicit formatA: JsonFormat[A], interpolator: ValueInterpolator[A]): RootJsonFormat[SurfacePointProperty[A]] = new RootJsonFormat[SurfacePointProperty[A]] {
    override def read(json: JsValue): SurfacePointProperty[A] = {
      val fields = json.asJsObject(s"expected SurfacePointProperty, got $json").fields

      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]
      val triangulation = TriangleList(triangles)

      val pointData = fields("pointData").convertTo[IndexedSeq[A]]

      SurfacePointProperty(
        triangulation = triangulation,
        pointData = pointData
      )
    }

    override def write(obj: SurfacePointProperty[A]): JsValue = {
      JsObject(
        "triangles" -> obj.triangulation.triangles.toJson,
        "pointData" -> obj.pointData.toJson,
        "@type" -> "SurfacePointProperty".toJson
      )
    }
  }

  implicit def trianglePropertyFormat[A](implicit formatA: JsonFormat[A]): RootJsonFormat[TriangleProperty[A]] = new RootJsonFormat[TriangleProperty[A]] {
    override def read(json: JsValue): TriangleProperty[A] = {
      val fields = json.asJsObject(s"expected TriangleProperty, got $json").fields

      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]
      val triangulation = TriangleList(triangles)

      val triangleData = fields("triangleData").convertTo[IndexedSeq[A]]

      TriangleProperty(
        triangulation = triangulation,
        triangleData = triangleData
      )
    }

    override def write(obj: TriangleProperty[A]): JsValue = {
      JsObject(
        "triangles" -> obj.triangulation.triangles.toJson,
        "triangleData" -> obj.triangleData.toJson,
        "@type" -> "TriangleProperty".toJson
      )
    }
  }

  implicit def vertexPropertyPerTriangleFormat[A](implicit formatA: JsonFormat[A], interpolator: ValueInterpolator[A]): RootJsonFormat[VertexPropertyPerTriangle[A]] = new RootJsonFormat[VertexPropertyPerTriangle[A]] {
    override def read(json: JsValue): VertexPropertyPerTriangle[A] = {
      val fields = json.asJsObject(s"expected VertexPropertyPerTriangle, got $json").fields

      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]
      val triangulation = TriangleList(triangles)

      val pointData = fields("pointData").convertTo[IndexedSeq[A]]
      val triangleIndex = fields("triangleIndex").convertTo[IndexedSeq[(Int, Int, Int)]].map{case(i, j, k) => IntVector3D(i, j, k)}

      VertexPropertyPerTriangle(triangulation, triangleIndex, pointData)
    }

    override def write(obj: VertexPropertyPerTriangle[A]): JsValue = JsObject(
      "triangles" -> obj.triangulation.triangles.toJson,
      "pointData" -> obj.vertexData.toJson,
      "triangleIndex" -> obj.triangleVertexIndex.map{v => (v.i, v.j, v.k)}.toJson,
      "@type" -> "VertexPropertyPerTriangle".toJson
    )
  }

  implicit def pixelImageFormat[A: ClassTag](implicit formatA: JsonFormat[A]): RootJsonFormat[PixelImage[A]] = new RootJsonFormat[PixelImage[A]] {
    override def read(json: JsValue): PixelImage[A] = {
      val fields = json.asJsObject(s"expected PixelImage, got $json").fields

      val width = fields("width").convertTo[Int]
      val height = fields("height").convertTo[Int]
      val data = fields("data").convertTo[IndexedSeq[A]]
      val domain = fields("domainMode").convertTo[String] match {
        case "ColumnMajor" => ColumnMajorImageDomain(width, height)
        case "RowMajor" => RowMajorImageDomain(width, height)
        case s: String => throw new DeserializationException(s"unknown image domain mode: $s")
      }

      PixelImage(domain, data).withAccessMode(AccessMode.Repeat())
    }

    override def write(obj: PixelImage[A]): JsValue = JsObject(
      "width" -> obj.width.toJson,
      "height" -> obj.height.toJson,
      "domainMode" -> (obj.domain match {
        case d: ColumnMajorImageDomain => "ColumnMajor"
        case d: RowMajorImageDomain => "RowMajor"
      }).toJson,
      "data" -> obj.values.toIndexedSeq.toJson
    )
  }

  implicit val point2DFormat: JsonFormat[Point[_2D]] = new JsonFormat[Point[_2D]] {
    override def write(vec: Point[_2D]): JsValue = JsArray(JsNumber(vec.x), JsNumber(vec.y))

    override def read(json: JsValue): Point[_2D] = json match {
      case JsArray(Seq(JsNumber(x), JsNumber(y))) => Point(x.toDouble, y.toDouble)
      case _ => deserializationError("Expected Point[_2D], got:" + json)
    }
  }

  implicit val colSpaceOps2D: ColorSpaceOperations[Point[_2D]] = new ColorSpaceOperations[Point[_2D]] {
    override def add(pix1: Point[_2D], pix2: Point[_2D]): Point[_2D] = (pix1.toVector + pix2.toVector).toPoint
    override def multiply(pix1: Point[_2D], pix2: Point[_2D]): Point[_2D] = Point(pix1.x * pix2.x, pix1.y * pix2.y)
    override def dot(pix1: Point[_2D], pix2: Point[_2D]): Double = pix1.toVector.dot(pix2.toVector)
    override def dimensionality: Int = 2
    override def scale(pix: Point[_2D], l: Double): Point[_2D] = (pix.toVector * l).toPoint
    override def zero: Point[_2D] = Point2D.origin
  }

  implicit def textureMappedPropertyFormat[A: ClassTag](implicit formatA: JsonFormat[A], interpolator: ValueInterpolator[A], colorSpaceOperations: ColorSpaceOperations[A]): RootJsonFormat[TextureMappedProperty[A]] = new RootJsonFormat[TextureMappedProperty[A]] {
    override def read(json: JsValue): TextureMappedProperty[A] = {
      val fields = json.asJsObject(s"expected TextureMappedProperty, got $json").fields

      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]
      val triangulation = TriangleList(triangles)

      val textureMapping = fields("textureMapping").convertTo[MeshSurfaceProperty[Point[_2D]]]
      val texture = fields("texture").convertTo[PixelImage[A]]

      TextureMappedProperty(triangulation, textureMapping, texture)
    }

    override def write(obj: TextureMappedProperty[A]): JsValue = JsObject(
      "triangles" -> obj.triangulation.triangles.toJson,
      "textureMapping" -> obj.textureMapping.toJson,
      "texture" -> obj.texture.toJson,
      "@type" -> "TextureMappedProperty".toJson
    )
  }

  implicit def indirectPropertyFormat[A: ClassTag](implicit formatA: JsonFormat[A], interpolator: ValueInterpolator[A], colorSpaceOperations: ColorSpaceOperations[A]): RootJsonFormat[IndirectProperty[A]] = new RootJsonFormat[IndirectProperty[A]] {
    override def read(json: JsValue): IndirectProperty[A] = {
      val fields = json.asJsObject(s"expected IndirectProperty, got $json").fields

      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]
      val triangulation = TriangleList(triangles)

      val properties = fields("properties").convertTo[IndexedSeq[MeshSurfaceProperty[A]]]
      val triangleIndirectionIndex = fields("triangleIndirectionIndex").convertTo[IndexedSeq[Int]]

      IndirectProperty(triangulation, triangleIndirectionIndex, properties)
    }

    override def write(obj: IndirectProperty[A]): JsValue = JsObject(
      "triangles" -> obj.triangulation.triangles.toJson,
      "properties" -> obj.properties.toJson,
      "triangleIndirectionIndex" -> obj.triangleIndirectionIndex.toJson,
      "@type" -> "IndirectProperty".toJson
    )
  }


  implicit def meshSurfaceProperty[A: ClassTag](implicit formatA: JsonFormat[A], interpolator: ValueInterpolator[A], colorSpaceOperations: ColorSpaceOperations[A]): RootJsonFormat[MeshSurfaceProperty[A]] = new RootJsonFormat[MeshSurfaceProperty[A]] {
    override def read(json: JsValue): MeshSurfaceProperty[A] = {
      val fields = json.asJsObject(s"expected MeshSurfaceProperty, got: $json").fields
      fields("@type") match {
        case JsString("SurfacePointProperty") => json.convertTo[SurfacePointProperty[A]]
        case JsString("TriangleProperty") => json.convertTo[TriangleProperty[A]]
        case JsString("IndirectProperty") => json.convertTo[IndirectProperty[A]]
        case JsString("VertexPropertyPerTriangle") => json.convertTo[VertexPropertyPerTriangle[A]]
        case JsString("TextureMappedProperty") => json.convertTo[TextureMappedProperty[A]]
        case _ => throw new DeserializationException(s"Unknown type of MeshSurfaceProperty")
      }
    }

    override def write(obj: MeshSurfaceProperty[A]): JsValue = obj match {
      case prop: SurfacePointProperty[A] => prop.toJson
      case prop: TriangleProperty[A] => prop.toJson
      case prop: IndirectProperty[A] => prop.toJson
      case prop: VertexPropertyPerTriangle[A] => prop.toJson
      case prop: TextureMappedProperty[A] => prop.toJson
      case _ => throw new SerializationException("cannot serialize MeshSurfaceProperty, unknown type")
    }
  }

  implicit val colorNormalMesh: RootJsonFormat[ColorNormalMesh3D] = new RootJsonFormat[ColorNormalMesh3D] {

    override def write(mesh: ColorNormalMesh3D): JsValue = JsObject(
      "points" -> mesh.shape.pointSet.points.map(_.toVector).toIndexedSeq.toJson,
      "color" -> mesh.color.toJson,
      "normals" -> mesh.normals.toJson,
      "triangles" -> mesh.shape.triangles.toJson,
      "@type" -> "ColorNormalMesh".toJson)

    override def read(json: JsValue): ColorNormalMesh3D = {
      val fields = json.asJsObject(s"expected Mesh object, got: $json").fields

      val points  = fields("points").convertTo[IndexedSeq[EuclideanVector[_3D]]].map(_.toPoint)
      val color   = fields("color").convertTo[MeshSurfaceProperty[RGBA]]
      val normals = fields("normals").convertTo[MeshSurfaceProperty[EuclideanVector[_3D]]]
      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]

      val triangulation = TriangleList(triangles)
      val shape = TriangleMesh3D(points, triangulation)

      ColorNormalMesh3D(shape, color, normals)
    }
  }

  implicit val vertexColorMesh: RootJsonFormat[VertexColorMesh3D] = new RootJsonFormat[VertexColorMesh3D] {

    override def write(mesh: VertexColorMesh3D): JsValue = JsObject(
      "points" -> mesh.shape.pointSet.points.map(_.toVector).toIndexedSeq.toJson,
      "color" -> mesh.color.pointData.toJson,
      "triangles" -> mesh.shape.triangles.toJson,
      "@type" -> "VertexColorMesh".toJson)

    override def read(json: JsValue): VertexColorMesh3D = {
      val fields = json.asJsObject(s"expected VertexColorMesh3D object, got: $json").fields

      val points  = fields("points").convertTo[IndexedSeq[EuclideanVector[_3D]]].map(_.toPoint)
      val color   = fields("color").convertTo[IndexedSeq[RGBA]]
      val triangles = fields("triangles").convertTo[IndexedSeq[TriangleCell]]

      val triangulation = TriangleList(triangles)
      val shape = TriangleMesh3D(points, triangulation)

      require(color.length == points.length)

      VertexColorMesh3D(shape, SurfacePointProperty(triangulation, color))
    }
  }

  implicit val meshColorNormalsFormat: RootJsonFormat[MeshColorNormals] = new RootJsonFormat[MeshColorNormals] {

    override def write(mesh: MeshColorNormals): JsValue = mesh.colorNormalMesh.toJson

    override def read(json: JsValue): MeshColorNormals = MeshColorNormals(json.convertTo[ColorNormalMesh3D])
  }

  implicit val meshVertexColorFormat: RootJsonFormat[MeshVertexColor] = new RootJsonFormat[MeshVertexColor] {

    override def write(mesh: MeshVertexColor): JsValue = mesh.vertexColorMesh3D.toJson

    override def read(json: JsValue): MeshVertexColor = MeshVertexColor(json.convertTo[VertexColorMesh3D])
  }

  // general object reader/writer
  implicit val renderObjectFormat: RootJsonFormat[RenderObject] = new RootJsonFormat[RenderObject] {

    override def write(renderObject: RenderObject): JsValue = renderObject match {
      case momo: MoMoInstance => MoMoExpressInstanceV2(momo).toJson
      case mF: MeshFile => mF.toJson
      case meshDirect: MeshColorNormals => meshDirect.colorNormalMesh.toJson
      case _ => throw new SerializationException(s"no JSON writer for RenderObject: $renderObject")
    }

    override def read(json: JsValue): RenderObject = {
      val fields = json.asJsObject(s"expected RenderObject, got: $json").fields
      fields("@type") match {
        case JsString("MoMoInstance") => json.convertTo[MoMoInstanceV2].toMoMoInstance
        case JsString("MoMoExpressInstance") => json.convertTo[MoMoExpressInstanceV2].toMoMoInstance
        case JsString("MeshFile") => json.convertTo[MeshFile]
        case JsString("ColorNormalMesh") => MeshColorNormals(json.convertTo[ColorNormalMesh3D])
        case _ => throw new DeserializationException("Unknown type of RenderObject")
      }
    }
  }

  // illumination

  override implicit val directionalLightFormat: RootJsonFormat[DirectionalLight] = new RootJsonFormat[DirectionalLight] {
    override def write(light: DirectionalLight): JsValue = JsObject(
      ("ambient", light.ambient.toJson),
      ("diffuse", light.diffuse.toJson),
      ("direction", light.direction.toJson),
      ("specular", light.specular.toJson),
      ("shininess", light.shininess.toJson),
      ("@type", JsString("DirectionalLight")))

    override def read(json: JsValue): DirectionalLight = {
      val fields = json.asJsObject(s"expected DirectionalLight object, got: $json").fields
      DirectionalLight(
        ambient = fields("ambient").convertTo[RGB],
        diffuse = fields("diffuse").convertTo[RGB],
        direction = fields("direction").convertTo[EuclideanVector[_3D]],
        specular = fields("specular").convertTo[RGB],
        shininess = fields("shininess").convertTo[Double])
    }
  }

  override implicit val sphericalHarmonicsLightFormat: RootJsonFormat[SphericalHarmonicsLight] = new RootJsonFormat[SphericalHarmonicsLight] {
    override def write(obj: SphericalHarmonicsLight): JsValue = JsObject(
      "coefficients" -> obj.coefficients.toJson,
      "@type" -> "SphericalHarmonicsLight".toJson
    )

    override def read(json: JsValue): SphericalHarmonicsLight = {
      val fields = json.asJsObject(s"expected Spherical Harmonics object, got: $json").fields
      SphericalHarmonicsLight(
        coefficients = fields("coefficients").convertTo[IndexedSeq[EuclideanVector[_3D]]]
      )
    }
  }

  implicit val illuminationFormat: RootJsonFormat[Illumination] = new RootJsonFormat[Illumination] {
    override def write(obj: Illumination): JsValue = obj match {
      case dl: DirectionalLight => dl.toJson
      case shl: SphericalHarmonicsLight => shl.toJson
      case _ => throw new SerializationException("Unknown type of Illumination")
    }

    override def read(json: JsValue): Illumination = {
      val fields = json.asJsObject(s"expected Illumination, got: $json").fields
      fields("@type") match {
        case JsString("DirectionalLight") => json.convertTo[DirectionalLight]
        case JsString("SphericalHarmonicsLight") => json.convertTo[SphericalHarmonicsLight]
        case _ => throw new DeserializationException("Unknown type of Illumination")
      }
    }
  }

  private case class CameraV2(far: Double,
                              focalLength: Double,
                              near: Double,
                              orthographic: Boolean,
                              pitch: Double,
                              principalPoint: EuclideanVector[_2D],
                              roll: Double,
                              sensorSize: EuclideanVector[_2D],
                              skewFactor: Double,
                              translation: EuclideanVector[_3D],
                              yaw: Double) {
    def toCamera(imageSize: ImageSize): Camera = {
      val pp = Point(2 * principalPoint.x/imageSize.width, 2 * principalPoint.y/imageSize.height)
      Camera(
        focalLength,
        pp,
        sensorSize,
        near = near,
        far = far,
        orthographic = orthographic
      )
    }

    def toView: ViewParameter = ViewParameter(
      translation = translation,
      pitch = pitch,
      yaw = yaw,
      roll = roll
    )
  }

  private object CameraV2 {
    def apply(cam: Camera, imageSize: ImageSize, view: ViewParameter): CameraV2 = {
      val pp = Point(0.5 * imageSize.width * cam.principalPoint.x, 0.5 * imageSize.height * cam.principalPoint.y)
      new CameraV2(
        far = cam.far,
        focalLength = cam.focalLength,
        near = cam.near,
        orthographic = cam.orthographic,
        pitch = view.pitch,
        principalPoint = pp.toVector,
        roll = view.roll,
        sensorSize = cam.sensorSize,
        skewFactor = 1.0,
        translation = view.translation,
        yaw = view.yaw)
    }
  }

  private implicit val camFormatV2 = jsonFormat11(CameraV2.apply)

  override implicit val imageFormat: JsonFormat[ImageSize] = new JsonFormat[ImageSize] {
    override def write(img: ImageSize): JsValue = JsObject(
      "height" -> JsNumber(img.height),
      "width" -> JsNumber(img.width))

    override def read(json: JsValue): ImageSize = json.asJsObject.getFields("width", "height") match {
      case Seq(JsNumber(width), JsNumber(height)) => ImageSize(width.toInt, height.toInt)
      case _ => deserializationError("Expected Image, got:" + json)
    }
  }

  override implicit val colorFormat: RootJsonFormat[ColorTransform] = new RootJsonFormat[ColorTransform] {
    override def write(color: ColorTransform): JsValue = JsObject(
      ("gain", color.gain.toJson),
      ("colorContrast", color.colorContrast.toJson),
      ("offset", color.offset.toJson))

    override def read(json: JsValue): ColorTransform = {
      val fields = json.asJsObject(s"expected Color object, got: $json").fields
      ColorTransform(
        gain = fields("gain").convertTo[RGB],
        colorContrast = fields("colorContrast").convertTo[Double],
        offset = fields("offset").convertTo[RGB])
    }
  }
  // render parameter reader / writer
  override implicit val renderParameterFormat: RootJsonFormat[RenderParameter] = new RootJsonFormat[RenderParameter] {

    override def write(param: RenderParameter): JsValue = {

      val camV2 = CameraV2(param.camera, param.imageSize, param.view)
      val momoV2 = MoMoExpressInstanceV2(param.momo)

      val illumination: Illumination = if (param.environmentMap.nonEmpty) param.environmentMap else param.directionalLight

      JsObject(
        "pose" -> param.pose.toJson,
        "camera" -> camV2.toJson,
        "illumination" -> illumination.toJson,
        "renderObject" -> momoV2.toJson,
        "image" -> param.imageSize.toJson,
        "color" -> param.colorTransform.toJson,
        "version" -> version.toJson
      )
    }

    override def read(json: JsValue): RenderParameter = {
      val fields = json.asJsObject(s"expected BetterRenderParameter object, got: $json").fields
      fields("version") match {
        case JsString(`version`) =>
          val imageSize = fields("image").convertTo[ImageSize]
          val camV2 = fields("camera").convertTo[CameraV2]
          val momoV2 = fields("renderObject").convertTo[RenderObject] match {
            case momo: MoMoInstance => momo
            case _ => throw new RuntimeException("does not support reading other objects than MoMoInstance/MoMoExpressInstance")
          }

          val shLight = fields("illumination").convertTo[Illumination] match {
            case sh: SphericalHarmonicsLight => sh
            case dirLight: DirectionalLight => SphericalHarmonicsLight.empty
          }

          val dirLight = fields("illumination").convertTo[Illumination] match {
            case sh: SphericalHarmonicsLight => DirectionalLight.off
            case dirLight: DirectionalLight => dirLight
          }

          RenderParameter(
            pose = fields("pose").convertTo[Pose],
            view = camV2.toView,
            camera = camV2.toCamera(imageSize),
            environmentMap = shLight,
            directionalLight = dirLight,
            momo = momoV2,
            imageSize = imageSize,
            colorTransform = fields("color").convertTo[ColorTransform]
          )
        case v => throw new DeserializationException(s"wrong version number, expected $version, got $v")
      }
    }
  }

}

object RenderParameterJSONFormatV2 extends RenderParameterJSONFormatV2
