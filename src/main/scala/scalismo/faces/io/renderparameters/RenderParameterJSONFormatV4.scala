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

import scalismo.faces.parameters._
import spray.json._

/** V4 file format */
trait RenderParameterJSONFormatV4 extends RenderParameterJSONFormatV3 {
  // based on format V3: updated MoMoInstance now always has expressions, MoMoExpressInstance removed

  override val version = "V4.0"

  // momo instance writer and reader: direct format now
  override implicit val momoInstanceFormat: RootJsonFormat[MoMoInstance] = new RootJsonFormat[MoMoInstance] {
    def write(momo: MoMoInstance): JsValue = JsObject(
      ("shape", momo.shape.toJson),
      ("color", momo.color.toJson),
      ("expression", momo.expression.toJson),
      ("modelURI", momo.modelURI.toJson))

    def read(json: JsValue): MoMoInstance = {
      val fields = json.asJsObject(s"expected MoMoInstance object, got: $json").fields
      MoMoInstance(
        shape = fields("shape").convertTo[IndexedSeq[Double]],
        color = fields("color").convertTo[IndexedSeq[Double]],
        expression = fields("expression").convertTo[IndexedSeq[Double]],
        modelURI = fields("modelURI").convertTo[URI])
    }
  }

  // render parameter reader / writer
  override implicit val renderParameterFormat: RootJsonFormat[RenderParameter] = new RootJsonFormat[RenderParameter] {
    override def write(param: RenderParameter): JsValue = JsObject(
      "pose" -> param.pose.toJson,
      "view" -> param.view.toJson,
      "camera" -> param.camera.toJson,
      "environmentMap" -> param.environmentMap.toJson,
      "directionalLight" -> param.directionalLight.toJson,
      "momo" -> param.momo.toJson,
      "imageSize" -> param.imageSize.toJson,
      "colorTransform" -> param.colorTransform.toJson,
      "version" -> version.toJson
    )

    override def read(json: JsValue): RenderParameter = {
      val fields = json.asJsObject(s"expected BetterRenderParameter object, got: $json").fields
      fields("version") match {
        case JsString(`version`) => RenderParameter(
          pose = fields("pose").convertTo[Pose],
          view = fields("view").convertTo[ViewParameter],
          camera = fields("camera").convertTo[Camera],
          environmentMap = fields("environmentMap").convertTo[SphericalHarmonicsLight],
          directionalLight = fields("directionalLight").convertTo[DirectionalLight],
          momo = fields("momo").convertTo[MoMoInstance],
          imageSize = fields("imageSize").convertTo[ImageSize],
          colorTransform = fields("colorTransform").convertTo[ColorTransform]
        )
        case v => throw new DeserializationException(s"wrong version number, expected $version, got $v")
      }
    }
  }
}

object RenderParameterJSONFormatV4 extends RenderParameterJSONFormatV4
