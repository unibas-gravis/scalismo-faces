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

import scalismo.faces.color.RGBA
import scalismo.faces.image.PixelImage
import scalismo.faces.momo.MoMo
import scalismo.faces.parameters.{ParametricRenderer, RenderParameter, SphericalHarmonicsLight}
import scalismo.mesh.SurfacePointProperty


case class DepthMapRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.Black) extends RenderFromCorrespondenceImage[RGBA](correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) {
  override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
    val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
    val depthMap = correspondenceImage.map{ px=>
      if(px.isDefined){
        val frag = px.get
        val tId = frag.triangleId
        val bcc = frag.worldBCC
        val mesh = frag.mesh

        val posModel = mesh.position(tId, bcc)
        val posEyeCoordinates = parameters.modelViewTransform(posModel)

        Some((parameters.view.eyePosition-posEyeCoordinates).norm)
      }else{
        None
      }
    }
    val values  = depthMap.values.toIndexedSeq.flatten
    val ma = values.max
    val mi = values.min
    val mami = ma-mi
    depthMap.map{d=>
      if(d.isEmpty)
        clearColor
      else {
        RGBA(1.0 - (d.get - mi)/mami)
      }
    }
  }
}

case class NormalMapRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, clearColor: RGBA = RGBA.Black) extends RenderFromCorrespondenceImage[RGBA](correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) {
  override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
    val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
    val normalMap = correspondenceImage.map { px =>
      if (px.isDefined) {
        val frag = px.get
        val tId = frag.triangleId
        val bcc = frag.worldBCC
        val mesh = frag.mesh
        val normal = parameters.modelViewTransform(mesh.vertexNormals(tId, bcc))

        Some(normal)
      } else {
        None
      }
    }

    normalMap.map(n =>
      if (n.isEmpty)
        clearColor
      else {
        val v = n.get * 0.5
        RGBA(v.x - 0.5, v.y - 0.5, v.z - 0.5)
      })
  }
}

case class AlbedoRenderer(override val model: MoMo, override val clearColor: RGBA = RGBA.Black) extends MoMoRenderer(model: MoMo, clearColor: RGBA){

  /** render the albedo described by the parameters */
  override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
    val inst = instance(parameters)
    val ambientParameters = parameters.withEnvironmentMap(SphericalHarmonicsLight.ambientWhite)
    ParametricRenderer.renderParameterVertexColorMesh(
      ambientParameters,
      inst,
      clearColor)
  }

}

case class IlluminationRenderer(override val model: MoMo, override val clearColor: RGBA = RGBA.Black) extends MoMoRenderer(model: MoMo, clearColor: RGBA){

  /** render the illumination component described by the parameters */
  override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
    val inst = instance(parameters)
    val noColorInst = inst.copy(color = SurfacePointProperty(inst.shape.triangulation, inst.color.pointData.map(_ => RGBA(0.5, 0.5, 0.5))))
    ParametricRenderer.renderParameterVertexColorMesh(
      parameters,
      noColorInst,
      clearColor)
  }

}

