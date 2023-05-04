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
import scalismo.faces.io.renderparameters.fromats.DirectLightLegacySerializationFormat
import scalismo.faces.parameters.*
import scalismo.faces.mesh.*
import scalismo.geometry.*

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import upickle.default.{read, writeJs as write}

object RenderParameterJSONFormatLegacy extends RenderParameterRootJsonFormat {

  override val version = "1.0"

  import scalismo.faces.io.renderparameters.fromats.RenderParameterJSONFormats.directionalLightMapper
  import scalismo.faces.io.renderparameters.fromats.RenderParameterJSONFormats.poseMapper

  import fromats.SphericalHarmonicsLegacyFormat
  import fromats.MoMoInstanceLegacyFormat
  import fromats.LegacyCameraFormat
  import fromats.ColorTransformLegacyFormat

  implicit val imageSizeLegacyMapper: upickle.default.ReadWriter[ImageSize] = upickle.default
    .readwriter[List[Int]]
    .bimap[ImageSize](
      is => List(is.height, is.width),
      l => ImageSize(l(1), l(0))
    )

  implicit val colorTransformLegacyMapper: upickle.default.ReadWriter[ColorTransform] = upickle.default
    .readwriter[ColorTransformLegacyFormat]
    .bimap[ColorTransform](
      ct => ColorTransformLegacyFormat(ct),
      ctl => ctl.toColorTransform()
    )

  case class RenderParameterLegacy(
    camera: LegacyCameraFormat,
    color: ColorTransformLegacyFormat,
    directionalLight: DirectLightLegacySerializationFormat,
    image: ImageSize,
    morphableModel: MoMoInstanceLegacyFormat,
    pose: Pose,
    sphericalHarmonicsLight: SphericalHarmonicsLegacyFormat
  ) {
    def toRenderParameter(): RenderParameter = {
      RenderParameter(
        pose = pose,
        view = camera.toView,
        camera = camera.toCamera(image),
        environmentMap = sphericalHarmonicsLight.toSphericalHarmonics(),
        directionalLight =
          if (sphericalHarmonicsLight.coefficients.nonEmpty)
            DirectionalLight.off
          else
            directionalLight.toDirectionalLight(morphableModel.shininess),
        momo = morphableModel.toMoMoInstance,
        imageSize = image,
        colorTransform = color.toColorTransform()
      )
    }
  }

  object RenderParameterLegacy {
    def apply(rps: RenderParameter) = new RenderParameterLegacy(
      LegacyCameraFormat(rps.camera, rps.imageSize, rps.view),
      ColorTransformLegacyFormat(rps.colorTransform),
      if (rps.environmentMap.coefficients.nonEmpty)
        DirectLightLegacySerializationFormat(DirectionalLight.off)
      else
        DirectLightLegacySerializationFormat(rps.directionalLight),
      rps.imageSize,
      MoMoInstanceLegacyFormat(rps.momo, rps.directionalLight.shininess),
      rps.pose,
      SphericalHarmonicsLegacyFormat(rps.environmentMap)
    )

    implicit val rpsLegacyMapper: upickle.default.ReadWriter[RenderParameterLegacy] = upickle.default.macroRW

  }

  implicit val rpsMapper: upickle.default.ReadWriter[RenderParameter] = upickle.default
    .readwriter[RenderParameterLegacy]
    .bimap[RenderParameter](
      rps => RenderParameterLegacy(rps),
      rpsLegacy => rpsLegacy.toRenderParameter()
    )
}
