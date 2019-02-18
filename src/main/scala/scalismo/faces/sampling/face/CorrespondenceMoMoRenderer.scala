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

import scalismo.color.RGBA
import scalismo.faces.image.PixelImage
import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.mesh.VertexColorMesh3D
import scalismo.faces.momo.MoMo
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.render.TriangleRenderer.TriangleFragment
import scalismo.faces.render.{PixelShader, TriangleFilters, TriangleRenderer, ZBuffer}
import scalismo.geometry.{EuclideanVector, _3D}
import scalismo.mesh.{MeshSurfaceProperty, SurfacePointProperty}
import scalismo.utils.Memoize

/** Render correspondence images. Caches rasterization separately from shading. */
class CorrespondenceMoMoRenderer(override val model: MoMo, override val clearColor: RGBA) extends MoMoRenderer(model, clearColor) {

  def renderCorrespondenceImage(parameters: RenderParameter): PixelImage[Option[TriangleFragment]] = {
    val inst = instance(parameters)
    val buffer = ZBuffer[Option[TriangleFragment]](parameters.imageSize.width, parameters.imageSize.height, None)
    val worldMesh = inst.shape.transform(parameters.modelViewTransform.apply)
    val backfaceCullingFilter = TriangleFilters.backfaceCullingFilter(worldMesh, parameters.view.eyePosition)
    TriangleRenderer.renderCorrespondence(inst.shape, backfaceCullingFilter, parameters.pointShader, parameters.imageSize.screenTransform, buffer).toImage
  }

  /** render the image described by the parameters */
  override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
    val correspondenceImage = renderCorrespondenceImage(parameters)
    val inst = instance(parameters)
    val shader: PixelShader[RGBA] = parameters.pixelShader(inst)
    correspondenceImage.map{ px => if(px.isDefined) shader(px.get) else clearColor }
  }

  /** get a cached version of this renderer */
  override def cached(cacheSize: Int) = new CorrespondenceMoMoRenderer(model, clearColor) {
    private val imageRenderer = Memoize(super.renderImage, cacheSize)
    private val correspondenceImageRenderer = Memoize(super.renderCorrespondenceImage, cacheSize)
    private val meshRenderer = Memoize(super.renderMesh, cacheSize)
    private val maskRenderer = Memoize((super.renderMask _).tupled, cacheSize)
    private val lmRenderer = Memoize((super.renderLandmark _).tupled, cacheSize * allLandmarkIds.length)
    private val instancer = Memoize(super.instance, cacheSize)

    override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = imageRenderer(parameters)
    override def renderCorrespondenceImage(parameters: RenderParameter): PixelImage[Option[TriangleFragment]] = correspondenceImageRenderer(parameters)
    override def renderLandmark(lmId: String, parameter: RenderParameter): Option[TLMSLandmark2D] = lmRenderer((lmId, parameter))
    override def renderMesh(parameters: RenderParameter): VertexColorMesh3D = meshRenderer(parameters)
    override def instance(parameters: RenderParameter): VertexColorMesh3D = instancer(parameters)
    override def renderMask(parameters: RenderParameter, mask: MeshSurfaceProperty[Int]): PixelImage[Int] = maskRenderer((parameters, mask))
  }
}

object CorrespondenceMoMoRenderer {
  def apply(model: MoMo, clearColor: RGBA) = new CorrespondenceMoMoRenderer(model, clearColor)
  def apply(model: MoMo) = new CorrespondenceMoMoRenderer(model, RGBA.BlackTransparent)
}

abstract class RenderFromCorrespondenceImage[A](correspondenceMoMoRenderer: CorrespondenceMoMoRenderer) extends ParametricImageRenderer[A]{
  override def renderImage(parameters: RenderParameter): PixelImage[A]
}

case class CorrespondenceColorImageRenderer(correspondenceMoMoRenderer: CorrespondenceMoMoRenderer, backgroundColor: RGBA = RGBA.Black) extends RenderFromCorrespondenceImage[RGBA](correspondenceMoMoRenderer){
  val reference: VertexColorMesh3D = correspondenceMoMoRenderer.model.mean
  val normalizedReference: SurfacePointProperty[RGBA] = {
    val extent = reference.shape.pointSet.boundingBox.extent.toBreezeVector
    val min = reference.shape.pointSet.boundingBox.origin.toBreezeVector
    val extV = reference.shape.pointSet.boundingBox.extent
    val minV = reference.shape.pointSet.boundingBox.origin
    val normalizedPoints = reference.shape.pointSet.points.map(p => (p.toBreezeVector - min) /:/ extent).map(f => EuclideanVector[_3D](f.toArray)).toIndexedSeq
    SurfacePointProperty(reference.shape.triangulation, normalizedPoints.map(d=>RGBA(d.x,d.y, d.z)))
  }

  override def renderImage(parameters: RenderParameter): PixelImage[RGBA] = {
    val correspondenceImage = correspondenceMoMoRenderer.renderCorrespondenceImage(parameters)
    correspondenceImage.map{px =>
      if(px.isDefined) {
        val frag = px.get
        normalizedReference(frag.triangleId, frag.worldBCC)
      }else {
        backgroundColor
      }
    }
  }
}