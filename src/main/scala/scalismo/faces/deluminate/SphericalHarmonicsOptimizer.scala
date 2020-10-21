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

package scalismo.faces.deluminate


import scalismo.color.RGBA
import scalismo.faces.deluminate.SphericalHarmonicsSolver.IlluminatedPoint
import scalismo.faces.image.PixelImage
import scalismo.faces.parameters.{RenderParameter, SphericalHarmonicsLight}
import scalismo.faces.render.{ColorTransform, TextureExtraction}
import scalismo.faces.sampling.face.ParametricModel
import scalismo.geometry.{EuclideanVector, _3D}
import scalismo.mesh._
import scalismo.utils.Random

/** wrap Spherical Harmonics illumination solver in parametric rendering framework */
class SphericalHarmonicsOptimizer(val renderer: ParametricModel,
                                  val targetImage: PixelImage[RGBA])(implicit val rnd: Random) {

  private def targetAsSurfaceProperty(rps: RenderParameter): MeshSurfaceProperty[Option[RGBA]] = {
    TextureExtraction.imageAsSurfaceProperty(renderer.instance(rps).shape, rps.pointShader, targetImage)
  }

  /**
    * We solve for the environment map given the target radiance, Albedo and Normals.
    * We go after a subset of surface points only.
    */
  def optimize(rps: RenderParameter,
               surfacePointsSampler: TriangleMesh3D => IndexedSeq[(TriangleId, BarycentricCoordinates)]): SphericalHarmonicsLight = {
    val mesh: VertexColorMesh3D = renderer.instance(rps)
    val surfacePoints: IndexedSeq[(TriangleId, BarycentricCoordinates)] = surfacePointsSampler(mesh.shape)
    optimize(rps,surfacePoints)
  }


  def optimize(rps: RenderParameter,
               surfacePoints: IndexedSeq[(TriangleId, BarycentricCoordinates)]): SphericalHarmonicsLight = {
    val mesh: VertexColorMesh3D = renderer.instance(rps)
    val albedo = mesh.color
    val normals = mesh.shape.vertexNormals

    // radiance on surface
    val inverseColorTransform: ColorTransform = rps.colorTransform.transform.invert
    val imageAsProperty: MeshSurfaceProperty[Option[RGBA]] = targetAsSurfaceProperty(rps)

    // get surface points to evaluate from sampler


    //illuminated point set of sample points, note, there are invisible points which are automatically removed (via Option/None)
    val points: IndexedSeq[IlluminatedPoint] = surfacePoints.flatMap {
      case (tid, bcc) =>
        val r = imageAsProperty(tid, bcc).map(imageColor => {
          // Option because we could sample from invisible point
          val albedoLocal = albedo(tid, bcc)
          val normal = rps.pose.transform(normals(tid, bcc)).normalize
          IlluminatedPoint(normal, inverseColorTransform(imageColor.toRGB), albedoLocal.toRGB)
        })
        r
    }
    if(points.nonEmpty) { //If the face is outside the face This is cannot be done outside the function, because number of points depends on visibility.
    val lightField: IndexedSeq[EuclideanVector[_3D]]
    = SphericalHarmonicsSolver.solveSHSystemDeconvolve(points, SphericalHarmonicsLight.lambertKernel)
      SphericalHarmonicsLight(lightField)
    }else {
      rps.environmentMap

    }
  }

  override def toString = "SphericalHarmonicsOptimizer"
}

object SphericalHarmonicsOptimizer {
  def apply(renderer: ParametricModel, targetImage: PixelImage[RGBA])(implicit rnd: Random) = new SphericalHarmonicsOptimizer(renderer, targetImage)
}
