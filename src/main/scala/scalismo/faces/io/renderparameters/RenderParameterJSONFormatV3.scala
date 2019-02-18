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

import scalismo.faces.parameters._
import scalismo.geometry._
import spray.json._

trait RenderParameterJSONFormatV3 extends RenderParameterJSONFormatV2 {
  // based on format V2

  override val version = "V3.0"

  implicit val viewParameterFormat: RootJsonFormat[ViewParameter] = new RootJsonFormat[ViewParameter] {
    override def write(p: ViewParameter): JsValue = JsObject(
      ("translation", p.translation.toJson),
      ("roll",p.roll.toJson),
      ("yaw", p.yaw.toJson),
      ("pitch", p.pitch.toJson))

    override def read(json: JsValue): ViewParameter = {
      val fields = json.asJsObject(s"expected Pose object, got: $json").fields
      ViewParameter(
        translation = fields("translation").convertTo[EuclideanVector[_3D]],
        roll = fields("roll").convertTo[Double],
        yaw = fields("yaw").convertTo[Double],
        pitch = fields("pitch").convertTo[Double])
    }
  }

  implicit val cameraFormat: RootJsonFormat[Camera] = new RootJsonFormat[Camera] {
    override def write(cam: Camera): JsValue = JsObject(
      ("focalLength", cam.focalLength.toJson),
      ("principalPoint", cam.principalPoint.toVector.toJson),
      ("sensorSize", cam.sensorSize.toJson),
      ("near", cam.near.toJson),
      ("far", cam.far.toJson),
      ("orthographic", cam.orthographic.toJson))

    override def read(json: JsValue): Camera = {
      val fields = json.asJsObject(s"expected Camera object, got: $json").fields
      Camera(
        focalLength = fields("focalLength").convertTo[Double],
        sensorSize = fields("sensorSize").convertTo[EuclideanVector[_2D]],
        principalPoint = fields("principalPoint").convertTo[EuclideanVector[_2D]].toPoint,
        near = fields("near").convertTo[Double],
        far = fields("far").convertTo[Double],
        orthographic = fields("orthographic").convertTo[Boolean])
    }
  }

  // render parameter reader / writer
  override implicit val renderParameterFormat: RootJsonFormat[RenderParameter] = new RootJsonFormat[RenderParameter] {
    override def write(param: RenderParameter): JsValue = {
      val illumination: Illumination = if (param.environmentMap.nonEmpty) param.environmentMap else param.directionalLight

      JsObject(
        "pose" -> param.pose.toJson,
        "view" -> param.view.toJson,
        "camera" -> param.camera.toJson,
        "illumination" -> illumination.toJson,
        "renderObject" -> MoMoExpressInstanceV2(param.momo).toJson,
        "imageSize" -> param.imageSize.toJson,
        "colorTransform" -> param.colorTransform.toJson,
        "version" -> version.toJson
      )
    }

    override def read(json: JsValue): RenderParameter = {
      val fields = json.asJsObject(s"expected BetterRenderParameter object, got: $json").fields

      val momo = fields("renderObject").convertTo[RenderObject] match {
        case m: MoMoInstance => m
        case _ => throw new RuntimeException("cannot read other object than MoMoInstance/MoMoExpressInstance")
      }

      val shLight = fields("illumination").convertTo[Illumination] match {
        case sh: SphericalHarmonicsLight => sh
        case dirLight: DirectionalLight => SphericalHarmonicsLight.empty
      }

      val dirLight = fields("illumination").convertTo[Illumination] match {
        case sh: SphericalHarmonicsLight => DirectionalLight.off
        case dirLight: DirectionalLight => dirLight
      }

      fields("version") match {
        case JsString(`version`) => RenderParameter(
          pose = fields("pose").convertTo[Pose],
          view = fields("view").convertTo[ViewParameter],
          camera = fields("camera").convertTo[Camera],
          environmentMap = shLight,
          directionalLight = dirLight,
          momo = momo,
          imageSize = fields("imageSize").convertTo[ImageSize],
          colorTransform = fields("colorTransform").convertTo[ColorTransform]
        )
        case v => throw new DeserializationException(s"wrong version number, expected $version, got $v")
      }
    }
  }

  // ** scene parameter part **

  implicit val sceneTreeFormat: RootJsonFormat[SceneTree] = new RootJsonFormat[SceneTree] {
    override def read(json: JsValue): SceneTree = {
      val fields = json.asJsObject(s"expected SceneTree, got $json").fields
      fields("@type") match {
        case JsString("SceneObject") => SceneObject(fields("renderObject").convertTo[RenderObject])
        case JsString("PoseNode") => PoseNode(
          pose = fields("pose").convertTo[Pose],
          children = fields("children").convertTo[IndexedSeq[SceneTree]]
        )
        case _ => throw new DeserializationException(s"unknown type of SceneTree node")
      }
    }

    override def write(obj: SceneTree): JsValue = obj match {
      case PoseNode(pose, children) => JsObject(
        "pose" -> pose.toJson,
        "children" -> children.toJson,
        "@type" -> "PoseNode".toJson
      )
      case SceneObject(renderObject) => JsObject(
        "renderObject" -> renderObject.toJson,
        "@type" -> "SceneObject".toJson
      )
    }
  }

  implicit val sceneParameterFormat: RootJsonFormat[SceneParameter] = new RootJsonFormat[SceneParameter] {
    val version = "SceneParameterV3.0"
    override def write(param: SceneParameter): JsValue = JsObject(
      "view" -> param.view.toJson,
      "camera" -> param.camera.toJson,
      "illuminations" -> param.illuminations.toJson,
      "sceneTree" -> param.sceneTree.toJson,
      "imageSize" -> param.imageSize.toJson,
      "colorTransform" -> param.colorTransform.toJson,
      "version" -> version.toJson
    )

    override def read(json: JsValue): SceneParameter = {
      val fields = json.asJsObject(s"expected SceneParameter object, got: $json").fields
      fields("version") match {
        case JsString(`version`) => SceneParameter(
          view = fields("view").convertTo[ViewParameter],
          camera = fields("camera").convertTo[Camera],
          illuminations = fields("illuminations").convertTo[IndexedSeq[Illumination]],
          sceneTree = fields("sceneTree").convertTo[SceneTree],
          imageSize = fields("imageSize").convertTo[ImageSize],
          colorTransform = fields("colorTransform").convertTo[ColorTransform]
        )
        case v => throw new DeserializationException(s"wrong version number, expected $version, got $v")
      }
    }
  }

}

object RenderParameterJSONFormatV3 extends RenderParameterJSONFormatV3
