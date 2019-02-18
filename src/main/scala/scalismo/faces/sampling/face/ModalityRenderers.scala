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

import scalismo.color.{RGB, RGBA}
import scalismo.faces.image.PixelImage
import scalismo.faces.parameters.{ColorTransform, ParametricRenderer, RenderParameter}
import scalismo.geometry.{Point, EuclideanVector, _3D}
import scalismo.mesh.{BarycentricCoordinates, SurfacePointProperty, TriangleId}


object ModalityRenderers {

  object DepthMapRenderer {
    def apply(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) = new DepthMapRenderer(correspondenceMoMoRenderer)
  }
  class DepthMapRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) extends ParametricImageRenderer[Option[Double]] {

    override def renderImage(parameters: RenderParameter): PixelImage[Option[Double]] = {
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      correspondenceImage.map { optFrag =>
        optFrag.map { fragment =>
          parameters.renderTransform(fragment.mesh.position(fragment.triangleId, fragment.worldBCC)).z
        }
      }
    }

  }

  object NormalsRenderer {
    def apply(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) = new NormalsRenderer(correspondenceMoMoRenderer)
  }
  class NormalsRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) extends ParametricImageRenderer[Option[EuclideanVector[_3D]]] {

    override def renderImage(parameters: RenderParameter): PixelImage[Option[EuclideanVector[_3D]]] = {
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      correspondenceImage.map { optFrag =>
        optFrag.map { fragment =>
          parameters.modelViewTransform(fragment.mesh.vertexNormals(fragment.triangleId, fragment.worldBCC))
        }
      }
    }

    def renderNormalsVisualization(parameters: RenderParameter, clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
      val normals = renderImage(parameters)
      colorNormalImage(normals, clearColor)
    }
  }

  object AlbedoRenderer{
    def apply(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.BlackTransparent) = new AlbedoRenderer(correspondenceMoMoRenderer, clearColor)
  }
  class AlbedoRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA) extends ParametricImageRenderer[RGBA] {

    override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
      val instance = correspondenceMoMoRenderer.instance(parameters)
      val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
      correspondenceImage.map { optFrag =>
        optFrag.map { fragment =>
          instance.color.onSurface(fragment.triangleId, fragment.worldBCC)
        }.getOrElse(clearColor)
      }
    }
  }

  object IlluminationVisualizationRenderer {
    def apply(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.BlackTransparent) = new IlluminationVisualizationRenderer(correspondenceMoMoRenderer, clearColor)
  }
  class IlluminationVisualizationRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA) extends ParametricImageRenderer[RGBA] {

    override def renderImage(parameters: RenderParameter) : PixelImage[RGBA] = {
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

  def colorNormalImage(normals: PixelImage[Option[EuclideanVector[_3D]]], clearColor: RGBA) : PixelImage[RGBA] = {
    normals.map { opt =>
      opt.map { normal =>
        val v = normal * 0.5
        RGBA(0.5 - v.x, 0.5 - v.y, 0.5 - v.z, 1.0)
      }.getOrElse(clearColor)
    }
  }

}
