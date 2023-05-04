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
import scalismo.faces.image.*
import scalismo.faces.mesh.*
import scalismo.faces.parameters.*
import scalismo.geometry.*
import scalismo.mesh.*
import scalismo.numerics.ValueInterpolator

import scalismo.faces.io.renderparameters.fromats.RenderParameterJSONFormats._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

import upickle.default.{read, writeJs as write}

object RenderParameterJSONFormatV2 extends RenderParameterRootJsonFormat {

  override val version = "V2.0"

  import fromats.ColorTransformLegacyFormat
  import scalismo.faces.io.renderparameters.fromats.RenderParameterJSONFormats.imageSizeMapper
  import scalismo.faces.io.renderparameters.fromats.CameraFormatV2
  import scalismo.faces.io.renderparameters.fromats.MeshFileFormat

  implicit val illuminationMapper: upickle.default.ReadWriter[Illumination] = upickle.default
    .readwriter[ujson.Value]
    .bimap[Illumination](
      {
        case dl: DirectionalLight => {
          val js = write(dl)
          js("@type") = "DirectionalLight"
          js
        }
        case shl: SphericalHarmonicsLight => {
          val js = write(shl)
          js("@type") = "SphericalHarmonicsLight"
          js
        }
      },
      j => {
        val typeValue = j("@type").str
        typeValue match {
          case "DirectionalLight"        => read[DirectionalLight](j)
          case "SphericalHarmonicsLight" => read[SphericalHarmonicsLight](j)
        }
      }
    )

  implicit val renderObjectMapper: upickle.default.ReadWriter[RenderObject] = upickle.default
    .readwriter[ujson.Value]
    .bimap[RenderObject](
      ro =>
        ro match {
          case momo: MoMoInstance =>
            val mj = write(momo)
            if (momo.expression.nonEmpty) mj("@type") = "MoMoExpressInstance"
            else
              mj.obj -= "expression"
              mj("@type") = "MoMoInstance"
            mj
          case mF: MeshFile                 => write(mF)
          case meshDirect: MeshColorNormals => write(meshDirect.colorNormalMesh)
          case _ => throw new IllegalArgumentException(s"no JSON writer for RenderObject: $ro")
        },
      vj =>
        Try {
          vj("@type").str
        } match {
          case Success("MoMoInstance") =>
            val sanitized = vj
            sanitized("expression") = write(IndexedSeq[Double]())
            read[MoMoInstance](sanitized)
          case Success("MoMoExpressInstance") => read[MoMoInstance](vj)
          case Success("MeshFile")            => read[MeshFile](vj)
          case Success("ColorNormalMesh")     => MeshColorNormals(read[ColorNormalMesh3D](vj))
          case _ => throw new IllegalArgumentException("Unknown type of RenderObject: " + vj.toString)
        }
    )

  case class RenderParameterV2(pose: Pose,
                               camera: CameraFormatV2,
                               illumination: Illumination,
                               renderObject: RenderObject,
                               image: ImageSize,
                               color: ColorTransform,
                               version: String = version
  ) {
    def toRenderParameter(): RenderParameter = {
      val shLight = illumination match {
        case sh: SphericalHarmonicsLight => sh
        case _: DirectionalLight         => SphericalHarmonicsLight.empty
      }

      val dirLight = illumination match {
        case _: SphericalHarmonicsLight => DirectionalLight.off
        case dirLight: DirectionalLight => dirLight
      }

      val momo = renderObject match {
        case momo: MoMoInstance => momo
        case ro =>
          throw new RuntimeException(
            "does not support reading other objects than MoMoInstance/MoMoExpressInstance like " + ro.getClass.getSimpleName
          )
      }

      RenderParameter(
        pose = pose,
        view = camera.toView,
        camera = camera.toCamera(image),
        environmentMap = shLight,
        directionalLight = dirLight,
        momo = momo,
        imageSize = image,
        colorTransform = color
      )
    }
  }

  object RenderParameterV2 {
    def apply(rps: RenderParameter) = new RenderParameterV2(
      pose = rps.pose,
      camera = CameraFormatV2(rps.camera, rps.imageSize, rps.view),
      illumination =
        if (rps.environmentMap.nonEmpty)
          rps.environmentMap
        else
          rps.directionalLight,
      renderObject = rps.momo,
      image = rps.imageSize,
      color = rps.colorTransform
    )

    val renderParameterV2Mapper: upickle.default.ReadWriter[RenderParameterV2] = upickle.default.macroRW
    implicit val renderParameterV2GuardedMapper: upickle.default.ReadWriter[RenderParameterV2] = upickle.default
      .readwriter[ujson.Value]
      .bimap[RenderParameterV2](
        rpsV2 => write(rpsV2)(renderParameterV2Mapper),
        json => {
          Try {
            json("version").str
          } match {
            case Success(version) =>
              if (version != "V2.0")
                throw IllegalArgumentException(s"V2 json reader expects V2.0 json file, got: $version")
            case Failure(e) =>
              throw IllegalArgumentException(s"V2 json reader expects V2.0 json file, got: $e")
          }
          read[RenderParameterV2](json)(renderParameterV2Mapper)
        }
      )
  }

  implicit val rpsMapper: upickle.default.ReadWriter[RenderParameter] = upickle.default
    .readwriter[RenderParameterV2]
    .bimap[RenderParameter](
      rps => RenderParameterV2(rps),
      rpsV2 => rpsV2.toRenderParameter()
    )
}
