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

package scalismo.faces.mesh

import scalismo.mesh.{BarycentricCoordinates, MeshSurfaceProperty, TriangleId, TriangleMesh3D}
import scalismo.utils.Random

object MeshSurfaceSampling {
  def sampleUniformlyOnSurface(expectedAmountOfPoints: Int)(mesh: TriangleMesh3D)(implicit rnd: Random): IndexedSeq[(TriangleId, BarycentricCoordinates)] = {
    val meshArea = mesh.area
    def drawTriangle(tid: TriangleId): Boolean = {
      rnd.scalaRandom.nextDouble() < expectedAmountOfPoints * mesh.computeTriangleArea(mesh.triangulation.triangle(tid)) / meshArea
    }

    val triangleSubset = mesh.triangulation.triangleIds.filter(tid => drawTriangle(tid))
    triangleSubset.map(tid => {
      (tid, BarycentricCoordinates.randomUniform(rnd))
    })
  }

  /** Samples according to mask, a MeshSurfaceProperty. If it is 1 the point will be chosen uniformly on the surface if it is zero it will never be chosen. */
  def sampleAccordingToMask(mask: MeshSurfaceProperty[Double], expectedAmountOfPoints: Int)(mesh: TriangleMesh3D)(implicit rnd: Random): IndexedSeq[(TriangleId, BarycentricCoordinates)] = {
    val uniformSamples = sampleUniformlyOnSurface(expectedAmountOfPoints)(mesh)
    uniformSamples.filter { case (tid, bcc) => rnd.scalaRandom.nextDouble() < mask(tid, bcc) }
  }
}
