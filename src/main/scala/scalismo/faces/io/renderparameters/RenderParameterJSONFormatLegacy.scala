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

import scalismo.color.{RGB, RGBA}
import scalismo.faces.parameters._
import scalismo.faces.mesh._
import scalismo.geometry._

import spray.json._

/** legacy JSON format for RenderParameter, compatible to C++, uses spray's AST but no typeclass conversions */
trait RenderParameterJSONFormatLegacy extends DefaultJsonProtocol {
  val version = "V1.0"

  implicit val vector1DFormat: JsonFormat[EuclideanVector[_1D]] = new JsonFormat[EuclideanVector[_1D]] {
    override def write(vec: EuclideanVector[_1D]): JsValue = JsArray(JsNumber(vec.x))

    override def read(json: JsValue): EuclideanVector[_1D] = json match {
      case JsArray(Seq(JsNumber(x))) => EuclideanVector(x.toDouble)
      case _ => deserializationError("Expected EuclideanVector[_1D], got:" + json)
    }
  }

  implicit val vector2DFormat: JsonFormat[EuclideanVector[_2D]] = new JsonFormat[EuclideanVector[_2D]] {
    override def write(vec: EuclideanVector[_2D]): JsValue = JsArray(JsNumber(vec.x), JsNumber(vec.y))

    override def read(json: JsValue): EuclideanVector[_2D] = json match {
      case JsArray(Seq(JsNumber(x), JsNumber(y))) => EuclideanVector(x.toDouble, y.toDouble)
      case _ => deserializationError("Expected EuclideanVector[_2D], got:" + json)
    }
  }

  implicit val vector3DFormat: JsonFormat[EuclideanVector[_3D]] = new JsonFormat[EuclideanVector[_3D]] {
    override def write(vec: EuclideanVector[_3D]): JsValue = JsArray(JsNumber(vec.x), JsNumber(vec.y), JsNumber(vec.z))

    override def read(json: JsValue): EuclideanVector[_3D] = json match {
      case JsArray(Seq(JsNumber(x), JsNumber(y), JsNumber(z))) => EuclideanVector(x.toDouble, y.toDouble, z.toDouble)
      case _ => deserializationError("Expected EuclideanVector[_3D], got:" + json)
    }
  }

  implicit val rgbFormat: JsonFormat[RGB] = new JsonFormat[RGB] {
    override def write(col: RGB): JsValue = JsArray(JsNumber(col.r), JsNumber(col.g), JsNumber(col.b))

    override def read(json: JsValue): RGB = json match {
      case JsArray(Seq(JsNumber(r), JsNumber(g), JsNumber(b))) => RGB(r.toDouble, g.toDouble, b.toDouble)
      case _ => deserializationError(s"Expected RGB, got: $json")
    }
  }

  implicit val rgbaFormat: JsonFormat[RGBA] = new JsonFormat[RGBA] {
    override def write(col: RGBA): JsValue = JsArray(JsNumber(col.r), JsNumber(col.g), JsNumber(col.b), JsNumber(col.a))

    override def read(json: JsValue): RGBA = json match {
      case JsArray(Seq(JsNumber(r), JsNumber(g), JsNumber(b), JsNumber(a))) => RGBA(r.toDouble, g.toDouble, b.toDouble, a.toDouble)
      case _ => deserializationError(s"Expected RGBA, got: $json")
    }
  }

  implicit val uriFormat: JsonFormat[URI] = new JsonFormat[URI] {
    override def read(json: JsValue): URI = json match {
      case JsString(uri) => new URI(uri)
      case _ => throw DeserializationException(s"expected URI, got $json")
    }

    override def write(obj: URI): JsValue = JsString(obj.toString)
  }

  implicit val colorFormat: RootJsonFormat[ColorTransform] = new RootJsonFormat[ColorTransform] {
    override def write(color: ColorTransform): JsValue = JsObject(
      "gain" -> color.gain.toJson,
      "gamma" -> color.colorContrast.toJson,
      "offset" -> color.offset.toJson)

    override def read(json: JsValue): ColorTransform = {
      val fields = json.asJsObject(s"expected Color object, got: $json").fields
      ColorTransform(
        gain = fields("gain").convertTo[RGB],
        colorContrast = fields("gamma").convertTo[Double],
        offset = fields("offset").convertTo[RGB])
    }
  }

  implicit val poseFormat: RootJsonFormat[Pose] = new RootJsonFormat[Pose] {
    override def write(p: Pose): JsValue = JsObject(
      ("scaling", p.scaling.toJson),
      ("translation", p.translation.toJson),
      ("roll",p.roll.toJson),
      ("yaw", p.yaw.toJson),
      ("pitch", p.pitch.toJson))

    override def read(json: JsValue): Pose = {
      val fields = json.asJsObject(s"expected Pose object, got: $json").fields
      Pose(
        scaling = fields("scaling").convertTo[Double],
        translation = fields("translation").convertTo[EuclideanVector[_3D]],
        roll = fields("roll").convertTo[Double],
        yaw = fields("yaw").convertTo[Double],
        pitch = fields("pitch").convertTo[Double])
    }
  }

  private case class LegacyCamera(far: Double,
                                  focalLength: Double,
                                  near: Double,
                                  orthographic: Boolean,
                                  pitch: Double,
                                  principlePoint: EuclideanVector[_2D],
                                  roll: Double,
                                  sensorSize: EuclideanVector[_2D],
                                  skewFactor: Double,
                                  translation: EuclideanVector[_3D],
                                  yaw: Double) {
    def toCamera(imageSize: ImageSize): Camera = {
      val pp = Point(2 * principlePoint.x/imageSize.width, 2 * principlePoint.y/imageSize.height)
      Camera(
        focalLength,
        pp,
        sensorSize / near,
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

  private object LegacyCamera {
    def apply(cam: Camera, imageSize: ImageSize, view: ViewParameter): LegacyCamera = {
      val pp = Point(0.5 * imageSize.width * cam.principalPoint.x, 0.5 * imageSize.height * cam.principalPoint.y)
      new LegacyCamera(
        far = cam.far,
        focalLength = cam.focalLength,
        near = cam.near,
        orthographic = cam.orthographic,
        pitch = view.pitch,
        principlePoint = pp.toVector,
        roll = view.roll,
        sensorSize = cam.sensorSize * cam.near,
        skewFactor = 1.0,
        translation = view.translation,
        yaw = view.yaw)
    }
  }

  private implicit val legacyCameraFormat = jsonFormat11(LegacyCamera.apply)

  implicit val imageFormat: JsonFormat[ImageSize] = new JsonFormat[ImageSize] {
    override def write(img: ImageSize): JsValue = JsArray(JsNumber(img.height), JsNumber(img.width))

    override def read(json: JsValue): ImageSize = json match {
      // warning: order of width and height is not as expected!!
      case JsArray(Seq(JsNumber(height), JsNumber(width))) => ImageSize(width.toInt, height.toInt)
      case _ => deserializationError("Expected Image, got:" + json)
    }
  }

  implicit val directionalLightFormat: RootJsonFormat[DirectionalLight] = new RootJsonFormat[DirectionalLight] {
    override def write(light: DirectionalLight): JsValue = JsObject(
      ("ambient", light.ambient.toJson),
      ("diffuse", light.diffuse.toJson),
      ("direction", light.direction.toJson),
      ("specular", light.specular.toJson))

    override def read(json: JsValue): DirectionalLight = {
      val fields = json.asJsObject(s"expected DirectionalLight object, got: $json").fields
      DirectionalLight(
        ambient = fields("ambient").convertTo[RGB],
        diffuse = fields("diffuse").convertTo[RGB],
        direction = fields("direction").convertTo[EuclideanVector[_3D]],
        specular = fields("specular").convertTo[RGB])
    }
  }

  private case class MoMoInstanceLegacy(shapeCoefficients: IndexedSeq[Double],
                                colorCoefficients: IndexedSeq[Double],
                                modelURI: URI,
                                shininess: Double) {
    def toMoMoInstance = MoMoInstance(shapeCoefficients, colorCoefficients, IndexedSeq.empty, modelURI)
  }

  private object MoMoInstanceLegacy {
    def apply(momo: MoMoInstance, shininess: Double): MoMoInstanceLegacy = MoMoInstanceLegacy(momo.shape, momo.color, momo.modelURI, shininess)
  }

  private implicit val momoLegacyFormat: RootJsonFormat[MoMoInstanceLegacy] = jsonFormat4(MoMoInstanceLegacy.apply)

  implicit val sphericalHarmonicsLightFormat: RootJsonFormat[SphericalHarmonicsLight] = new RootJsonFormat[SphericalHarmonicsLight] {

    override def write(shl: SphericalHarmonicsLight): JsValue = {
      def convertSHLToJz(shl: IndexedSeq[EuclideanVector[_3D]]): IndexedSeq[EuclideanVector[_3D]] = {
        if (shl.isEmpty)
          shl
        else if (shl.size > 9)
          throw new SerializationException("SHL json writer (V1): cannot write more than 9 coefficients in jz format (use modern version)")
        else {
          // 1 - 9 coefficients
          val permutation = IndexedSeq(0, 3, 1, 2, 6, 5, 7, 4, 8).take(shl.size)
          permutation map (i => shl(i))
        }
      }

      convertSHLToJz(shl.coefficients).toJson
    }

    override def read(value: JsValue): SphericalHarmonicsLight = {
      // need to convert from V1 order to our own order (jz order -> ss order)
      def convertJzToSHL(shl: IndexedSeq[EuclideanVector[_3D]]): IndexedSeq[EuclideanVector[_3D]] = {
        if (shl.isEmpty)
          shl
        else if (shl.size > 9)
          throw DeserializationException("SHL json reader (V1): cannot read more than 2 bands (9 coeffs), there is no conversion from jz to our format")
        else {
          // 1 - 0 coeffs
          val permutation = IndexedSeq(0, 2, 3, 1, 7, 5, 4, 6, 8).take(shl.size)
          permutation map (i => shl(i))
        }
      }

      val shl = value.convertTo[IndexedSeq[EuclideanVector[_3D]]]
      SphericalHarmonicsLight(convertJzToSHL(shl))
    }
  }

  implicit val renderParameterFormat: RootJsonFormat[RenderParameter] = new RootJsonFormat[RenderParameter] {

    override def write(param: RenderParameter): JsValue = {

      val shLight = param.environmentMap
      val dirLight = if (shLight.nonEmpty) DirectionalLight.off else param.directionalLight

      val momoInst: MoMoInstanceLegacy = MoMoInstanceLegacy(param.momo, dirLight.shininess)

      val legacyCamera = LegacyCamera(param.camera, param.imageSize, param.view)

      JsObject(
        ("camera", legacyCamera.toJson),
        ("color", param.colorTransform.toJson),
        ("directionalLight", dirLight.toJson),
        ("image", param.imageSize.toJson),
        ("morphableModel", momoInst.toJson),
        ("pose", param.pose.toJson),
        ("sphericalHarmonicsLight", shLight.toJson)
      )
    }

    override def read(json: JsValue): RenderParameter = {
      val fields = json.asJsObject(s"expected RenderParameter object, got: $json").fields

      // V1.0: missing version field
      if (fields.contains("version")) {
        val version = fields("version").convertTo[String]
        if (version != "1.0")
          throw DeserializationException(s"V1 json reader expects V1.0 json file, got: $version")
      }

      // parse illumination: SHL overrides directed light
      val momoLegacy = fields("morphableModel").convertTo[MoMoInstanceLegacy]
      val momo = momoLegacy.toMoMoInstance

      val shLight = fields("sphericalHarmonicsLight").convertTo[SphericalHarmonicsLight]
      val dirLight = if (shLight.nonEmpty) DirectionalLight.off else fields("directionalLight").convertTo[DirectionalLight].copy(shininess = momoLegacy.shininess)

      val illumination = if (shLight.nonEmpty) shLight else dirLight

      val pose = fields("pose").convertTo[Pose]
      val imageSize = fields("image").convertTo[ImageSize]
      val color = fields("color").convertTo[ColorTransform]

      // legacy camera format
      val legCam = fields("camera").convertTo[LegacyCamera]
      val cam = legCam.toCamera(imageSize)
      val view = legCam.toView

      RenderParameter(
        pose = pose,
        view = view,
        camera = cam,
        environmentMap = shLight,
        directionalLight = dirLight,
        momo = momo,
        imageSize = imageSize,
        colorTransform = color
      )
    }
  }
}

object RenderParameterJSONFormatLegacy extends RenderParameterJSONFormatLegacy