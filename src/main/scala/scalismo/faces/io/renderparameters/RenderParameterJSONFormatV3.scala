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

import scalismo.color.{ColorSpaceOperations, RGBA}
import scalismo.faces.mesh.{IndirectProperty, TextureMappedProperty, VertexPropertyPerTriangle}
import scalismo.faces.parameters.*
import scalismo.geometry.*
import scalismo.numerics.ValueInterpolator
import scalismo.mesh.SurfacePointProperty

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import upickle.default.{read, writeJs as write, ReadWriter}

object RenderParameterJsonFormatV3 extends RenderParameterRootJsonFormat {

  override val version = "V3.0"

  implicit val uriMapper: ReadWriter[java.net.URI] = fromats.RenderParameterJSONFormats.uriMapper
  implicit val poseMapper: ReadWriter[Pose] = fromats.RenderParameterJSONFormats.poseMapper
  implicit val viewMapper: ReadWriter[ViewParameter] = fromats.RenderParameterJSONFormats.viewMapper
  implicit val cameraMapper: ReadWriter[Camera] = fromats.RenderParameterJSONFormats.cameraMapper
  implicit val imageSizeMapper: ReadWriter[ImageSize] = fromats.RenderParameterJSONFormats.imageSizeMapper
  implicit val colorTransformMapper: ReadWriter[ColorTransform] =
    fromats.RenderParameterJSONFormats.colorTransformMapper
  implicit val renderObjectMapper: ReadWriter[RenderObject] = RenderParameterJSONFormatV2.renderObjectMapper
  implicit val illuminationMapper: ReadWriter[Illumination] = RenderParameterJSONFormatV2.illuminationMapper

  implicit val momoMapper: upickle.default.ReadWriter[MoMoInstance] = upickle.default.macroRW

  implicit val rgbaMapper: ReadWriter[RGBA] = fromats.RenderParameterJSONFormats.rgbaMapper
  implicit val ev3Mapper: ReadWriter[EuclideanVector[_3D]] = fromats.RenderParameterJSONFormats.ev3DMapper

  implicit def surfacePropertyMapper[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A]
  ): ReadWriter[SurfacePointProperty[A]] = {
    fromats.RenderParameterJSONFormats.surfacePointPropertyFormat
  }

  implicit def textureMappedPropertyMapper[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A],
    colorSpaceOperations: ColorSpaceOperations[A]
  ): ReadWriter[TextureMappedProperty[A]] = {
    fromats.RenderParameterJSONFormats.textureMappedPropertyFormat
  }

  implicit def vertexPropertyPerTriangleMapper[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A],
    colorSpaceOperations: ColorSpaceOperations[A]
  ): ReadWriter[VertexPropertyPerTriangle[A]] = {
    fromats.RenderParameterJSONFormats.vertexPropertyPerTriangleFormat
  }

  implicit def indirectPropertyMapper[A: ClassTag](implicit
    formatA: ReadWriter[A],
    interpolator: ValueInterpolator[A],
    colorSpaceOperations: ColorSpaceOperations[A]
  ): ReadWriter[IndirectProperty[A]] = {
    fromats.RenderParameterJSONFormats.indirectPropertyFormat
  }

  implicit val meshVertexColorMapper: ReadWriter[MeshVertexColor] =
    fromats.RenderParameterJSONFormats.meshVertexColorFormat
  implicit val meshColorNormalsMapper: ReadWriter[MeshColorNormals] =
    fromats.RenderParameterJSONFormats.meshColorNormalsFormat
  implicit val meshFileMapper: ReadWriter[MeshFile] = fromats.RenderParameterJSONFormats.meshFileFormat

  case class RenderParameterV3(pose: Pose,
                               view: ViewParameter,
                               camera: Camera,
                               illumination: Illumination,
                               renderObject: RenderObject,
                               imageSize: ImageSize,
                               colorTransform: ColorTransform,
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
        case m: MoMoInstance => m
        case _ => throw new RuntimeException("cannot read other object than MoMoInstance/MoMoExpressInstance")
      }

      RenderParameter(
        pose = pose,
        view = view,
        camera = camera,
        environmentMap = shLight,
        directionalLight = dirLight,
        momo = momo,
        imageSize = imageSize,
        colorTransform = colorTransform
      )
    }
  }

  object RenderParameterV3 {
    def apply(rps: RenderParameter) = new RenderParameterV3(
      pose = rps.pose,
      view = rps.view,
      camera = rps.camera,
      illumination =
        if (rps.environmentMap.nonEmpty)
          rps.environmentMap
        else
          rps.directionalLight,
      renderObject = rps.momo,
      imageSize = rps.imageSize,
      colorTransform = rps.colorTransform
    )

    val renderParameterV3Mapper: upickle.default.ReadWriter[RenderParameterV3] = upickle.default.macroRW
    implicit val renderParameterV3GuardedMapper: upickle.default.ReadWriter[RenderParameterV3] = upickle.default
      .readwriter[ujson.Value]
      .bimap(
        rpsV3 => write(rpsV3)(renderParameterV3Mapper),
        json => {
          Try {
            json("version").str
          } match {
            case Success(version) =>
              if (version != "V3.0")
                throw IllegalArgumentException(s"V3 json reader expects V3.0 json file, got: $version")
            case Failure(e) =>
              throw IllegalArgumentException(s"V3 json reader expects V3.0 json file, got: $e")
          }
          read[RenderParameterV3](json)(renderParameterV3Mapper)
        }
      )
  }

  implicit val rpsMapper: upickle.default.ReadWriter[RenderParameter] = upickle.default
    .readwriter[RenderParameterV3]
    .bimap[RenderParameter](
      rps => RenderParameterV3(rps),
      rpsV3 => rpsV3.toRenderParameter()
    )

  implicit val sceneTreeFormat: ReadWriter[SceneTree] = upickle.default
    .readwriter[ujson.Value]
    .bimap[SceneTree](
      obj =>
        obj match {
          case PoseNode(pose, children) =>
            ujson.Obj(
              "pose" -> write(pose),
              "children" -> write(children),
              "@type" -> "PoseNode"
            )
          case SceneObject(renderObject) =>
            ujson.Obj(
              "renderObject" -> write(renderObject),
              "@type" -> "SceneObject"
            )
        },
      json => {
        json("@type").str match {
          case "SceneObject" => SceneObject(read[RenderObject](json("renderObject")))
          case "PoseNode" =>
            PoseNode(
              pose = read[Pose](json("pose")),
              children = read[IndexedSeq[SceneTree]](json("children"))
            )
          case _ => throw new IllegalArgumentException(s"unknown type of SceneTree node")
        }
      }
    )

  implicit val sceneParameterFormat: ReadWriter[SceneParameter] = {
    val version = "SceneParameterV3.0"

    upickle.default
      .readwriter[ujson.Value]
      .bimap[SceneParameter](
        param =>
          ujson.Obj(
            "view" -> write(param.view),
            "camera" -> write(param.camera),
            "illuminations" -> write(param.illuminations),
            "sceneTree" -> write(param.sceneTree),
            "imageSize" -> write(param.imageSize),
            "colorTransform" -> write(param.colorTransform),
            "version" -> version
          ),
        json => {
          json("version").str match {
            case version =>
              SceneParameter(
                view = read[ViewParameter](json("view")),
                camera = read[Camera](json("camera")),
                illuminations = read[IndexedSeq[Illumination]](json("illuminations")),
                sceneTree = read[SceneTree](json("sceneTree")),
                imageSize = read[ImageSize](json("imageSize")),
                colorTransform = read[ColorTransform](json("colorTransform"))
              )
            case v => throw new IllegalArgumentException(s"wrong version number, expected $version, got $v")
          }
        }
      )
  }
}
