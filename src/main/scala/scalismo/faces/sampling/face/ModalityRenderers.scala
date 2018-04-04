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

package scalismo.faces.sampling.face

import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.image.PixelImage
import scalismo.faces.parameters.{ColorTransform, ParametricRenderer, RenderParameter}
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.mesh.{BarycentricCoordinates, SurfacePointProperty, TriangleId}


object ModalityRenderers {

  object CorrespondenceMoMoModalityRenderer {

    def renderDepthMap(parameters: RenderParameter, correspondenceMoMoRenderer: CorrespondenceMoMoRenderer): PixelImage[Option[Double]] = {
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      correspondenceImage.map { optFrag =>
        optFrag.map { fragment =>
          parameters.renderTransform(fragment.mesh.position(fragment.triangleId, fragment.worldBCC)).z
        }
      }
    }

    def renderNormals(parameters: RenderParameter, correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) : PixelImage[Option[Vector[_3D]]] = {
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      correspondenceImage.map{ optFrag =>
        optFrag.map{ fragment =>
          parameters.modelViewTransform(fragment.mesh.vertexNormals(fragment.triangleId,fragment.worldBCC))
        }
      }
    }

    def renderNormalsImage(parameters: RenderParameter, correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.BlackTransparent) : PixelImage[RGBA] = {
      val normals = renderNormals(parameters, correspondenceMoMoRenderer)
      colorNormalImage(normals, clearColor)

    }

    def renderAlbedo(parameters: RenderParameter, correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
      val instance = correspondenceMoMoRenderer.instance(parameters)
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      correspondenceImage.map { optFrag =>
        optFrag.map { fragment =>
          instance.color.onSurface(fragment.triangleId, fragment.worldBCC)
        }.getOrElse(clearColor)
      }
    }

    def renderIlluminationImage(parameters: RenderParameter, correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.BlackTransparent) : PixelImage[RGBA] = {
      val instance = correspondenceMoMoRenderer.instance(parameters)
      val noColorInst = instance.copy(color = SurfacePointProperty(instance.shape.triangulation, instance.color.pointData.map(_ => RGBA(0.5, 0.5, 0.5))))
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      val shader = parameters.noColorTransform.pixelShader(noColorInst)
      correspondenceImage.map{ optFrag =>
        optFrag.map{ fragment =>
          shader(fragment.triangleId,fragment.worldBCC,Point(fragment.x,fragment.y,fragment.z))
        }.getOrElse(clearColor)
      }
    }

  }

  object ParametricModelModalityRenderer {

    def renderDepthMap(parameters: RenderParameter, parametricModel: ParametricModel, clearColor: Option[Double] = None): PixelImage[Option[Double]] = {
      val instance = parametricModel.instance(parameters)
      ParametricRenderer.renderParameter(parameters, (triangleId: TriangleId,
                                                      worldBCC: BarycentricCoordinates,
                                                      _: Point[_3D]) => {
        Some(parameters.renderTransform(instance.shape.position(triangleId, worldBCC)).z)
      }, clearColor)
    }

    def renderNormals(parameters: RenderParameter, parametricModel: ParametricModel, clearColor: Option[Vector[_3D]] = None): PixelImage[Option[Vector[_3D]]] = {
      val instance = parametricModel.instance(parameters)
      ParametricRenderer.renderParameter(parameters, (triangleId: TriangleId,
                                                      worldBCC: BarycentricCoordinates,
                                                      _: Point[_3D]) => {
        Some(parameters.modelViewTransform(instance.shape.vertexNormals(triangleId, worldBCC)))
      },
        clearColor)
    }

    def renderNormalsImage(parameters: RenderParameter, parametricModel: ParametricModel, clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
      val normals = renderNormals(parameters, parametricModel)
      colorNormalImage(normals, clearColor)
    }

    def renderAlbedo(parameters: RenderParameter, parametricModel: ParametricModel, clearColor: RGBA = RGBA.BlackTransparent) : PixelImage[RGBA] = {
      val instance = parametricModel.instance(parameters)
      ParametricRenderer.renderPropertyImage(parameters,
        instance.shape,
        instance.color).map(_.getOrElse(clearColor))
    }

    def renderIlluminationImage(parameters: RenderParameter, parametricModel: ParametricModel, clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
      val instance = parametricModel.instance(parameters)
      val noColorInst = instance.copy(color = SurfacePointProperty(instance.shape.triangulation, instance.color.pointData.map(_ => RGBA(0.5, 0.5, 0.5))))
      ParametricRenderer.renderParameterVertexColorMesh(
        parameters.noColorTransform,
        noColorInst,
        clearColor)
    }

  }



  def normalizeDepthMapIamge(depthMap: PixelImage[Option[Double]]) : PixelImage[Option[Double]] = {
    val values = depthMap.values.flatten.toIndexedSeq
    if (!values.isEmpty) {
      val max = values.max
      val min = values.min
      val mami = max-min
      depthMap.map(v => v.map(d => (d-min)/mami))
    } else {
      depthMap
    }
  }

  def normalizeDepthMapIamge(depthMap: PixelImage[Option[Double]], clearValue: Double) : PixelImage[Double] = {
    val values = depthMap.values.flatten.toIndexedSeq
    if (!values.isEmpty) {
      val max = values.max
      val min = values.min
      val mami = max-min
      depthMap.map(v => v.map(d => (d-min)/mami).getOrElse(clearValue))
    } else {
      depthMap.map(v => v.getOrElse(clearValue))
    }
  }

  def colorNormalImage(normals: PixelImage[Option[Vector[_3D]]]) : PixelImage[Option[RGBA]] = {
    normals.map(opt => opt.map { normal =>
      val v = normal * 0.5
      RGBA(0.5 - v.x, 0.5 - v.y, 0.5 - v.z, 1.0)
    })
  }

  def colorNormalImage(normals: PixelImage[Option[Vector[_3D]]], clearColor: RGBA) : PixelImage[RGBA] = {
    normals.map { opt =>
      opt.map { normal =>
        val v = normal * 0.5
        RGBA(0.5 - v.x, 0.5 - v.y, 0.5 - v.z, 1.0)
      }.getOrElse(clearColor)
    }
  }


}
