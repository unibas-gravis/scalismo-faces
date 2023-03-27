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

package scalismo.faces.manipulation

import scalismo.color.RGBA
import scalismo.faces.image.{PixelImage, PixelImageDomain}
import scalismo.faces.parameters.{ParametricRenderer, RenderParameter}
import scalismo.faces.render.TriangleRenderer
import scalismo.geometry.{Point, Point2D, EuclideanVector, _2D}
import scalismo.mesh.{SurfacePointProperty, TriangleMesh3D}
import scalismo.numerics.ValueInterpolator

/** render manipulations of face meshes */
class ManipulationRenderer {
  /** additive color transfer, transfer difference */
  def additiveTransfer(targetColor: RGBA, sourceRendering: RGBA, targetRendering: RGBA): RGBA = {
    RGBA(targetColor.toRGB + (targetRendering.toRGB - sourceRendering.toRGB), targetColor.a * sourceRendering.a)
  }

  /** multiplicative color transfer, transfer ratio */
  def multiplicativeTransfer(targetColor: RGBA, sourceRendering: RGBA, targetRendering: RGBA): RGBA = {
    targetColor x (targetRendering / sourceRendering.map(f => math.max(1e-5f, f)))
  }

  /** 2d point with a visibility */
  private case class PointWithVis(point: Point[_2D], visibility: Double)

  /** render points of mesh to 2d image locations, respects visibility */
  private def renderPointsIn2D(parameter: RenderParameter, mesh: TriangleMesh3D): IndexedSeq[PointWithVis] = {

    val surfaceVis = TriangleRenderer.visibilityAsSurfaceProperty(
      mesh,
      parameter.pointShader,
      PixelImageDomain(parameter.imageSize.width, parameter.imageSize.height),
      1e-5,
      boundaryAlwaysVisible = false
    ).map{vis => if (vis) 1.0 else 0.0}

    val pointVis =  SurfacePointProperty.sampleSurfaceProperty[Double](surfaceVis, list => list.head)

    mesh.pointSet.pointIds.map{id =>
      val pt = parameter.renderTransform(mesh.pointSet.point(id))
      PointWithVis(Point2D(pt.x, pt.y), pointVis.atPoint(id))
    }.toIndexedSeq
  }

  /** calculate the warpfield which turns a rendering of original into manipulation */
  def manipulationWarpField(originalParameter: RenderParameter,
                            originalMesh: TriangleMesh3D,
                            manipulatedParameter: RenderParameter,
                            manipulatedMesh: TriangleMesh3D): PixelImage[Option[EuclideanVector[_2D]]] = {
    require(originalMesh.pointSet.numberOfPoints == manipulatedMesh.pointSet.numberOfPoints)

    // render 2d positions of original and manipulation, respects visibility of points
    val points2DOriginal = renderPointsIn2D(originalParameter, originalMesh)
    val points2DManipulated = renderPointsIn2D(manipulatedParameter, manipulatedMesh)

    // warp vector with validity to remove warps involving invisible points
    case class WarpVector(vector: EuclideanVector[_2D], validity: Double)

    // warp list: warp vector for each point in mesh
    val warpList: IndexedSeq[WarpVector] = points2DOriginal.zip(points2DManipulated).map{
      case (PointWithVis(sourcePt, sourceVis), PointWithVis(targetPt, targetVis)) =>
        WarpVector(sourcePt - targetPt, sourceVis * targetVis)
    }

    // interpolator is required to render warp field as surface property
    implicit def warpInterpolator(implicit vecBlender: ValueInterpolator[EuclideanVector[_2D]], doubleBlender: ValueInterpolator[Double]): ValueInterpolator[WarpVector] =
      new ValueInterpolator[WarpVector] {
        override def blend(obj1: WarpVector, obj2: WarpVector, l: Double): WarpVector = {
          WarpVector(
            vecBlender.blend(obj1.vector, obj2.vector, l),
            doubleBlender.blend(obj1.validity, obj2.validity, l)
          )
        }
      }

    // render warpfield to 2d image (uses BCC interpolation, "property rendering")
    // respects validity and combines with invisible parts from rendering
    val rawWarpfield: PixelImage[Option[EuclideanVector[_2D]]] = ParametricRenderer.renderPropertyImage(
      manipulatedParameter,
      manipulatedMesh,
      SurfacePointProperty(manipulatedMesh.triangulation, warpList)
    ).map{
      // warp is only valid if it is visible in rendering and warps visible points (source and target need to be visible)
      case Some(WarpVector(vec, validity)) if validity > 0.5 => Some(vec)
      case _ => None
    }

    // raw warp field, still large regions probably undefined, further processing with WarpExtrapolator
    rawWarpfield
  }
}
