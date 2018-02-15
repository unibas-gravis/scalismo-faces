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

import breeze.linalg.CSCMatrix
import scalismo.common.PointId
import scalismo.mesh.{TriangleList, TriangleMesh3D}

/** Construct a discrete Lapalace-Beltrami operators from different types of weights. */
object DiscreteLaplaceBeltrami {

  trait LaplaceBeltramiWeightingFunction extends ((PointId, PointId) => Double)

  /**
    * Constructs a Laplace-Beltrami matrix from a given weight function where its rows and columns correspond to the PointIds of the given triangulation.
    *
    * @param triangulation
    * @param weightFun
    * @return
    */
  def laplaceBeltramiMatrix(triangulation: TriangleList, weightFun: LaplaceBeltramiWeightingFunction): CSCMatrix[Double] = {
    val n = triangulation.pointIds.length
    val builderW = new CSCMatrix.Builder[Double](n, n)
    for (i <- triangulation.pointIds) {
      val adj = triangulation.adjacentPointsForPoint(i)
      var sumW = 0.0
      for (j <- adj) {
        val w = weightFun(i, j)
        builderW.add(i.id, j.id, -w)
        sumW += w
      }
      val s = sumW
      builderW.add(i.id, i.id, s.toDouble)
    }
    builderW.result
  }

  /**
    * Constructs a cotangent weight function. w_ij = 0.5 * ( cot(alpha_ij) + cot(beta_ij) )
    *
    * Cotangent weight between two vertices x_i and x_j in a triangulated mesh is the sum of the cotangent of opposing angles alpha_1 and alpha_2.
    * Consider the two triangles:
    * .-------------------. x_i
    * - alpha_ij        -  -
    * -             -     -
    * -          -        -
    * -       -           -
    * -    -              -
    * - -          beta_ij -
    * x_j .-------------------.
    *
    * @param ref : Mesh used to calculate cotangent weights.
    * @return DenseMatrix[Double]
    */
  def cotangentWeight(ref: TriangleMesh3D): LaplaceBeltramiWeightingFunction = {

    /** All three cotangent values of a triangle are stored and added to possibly already existing values.
      * This way both cotan values are added.
      *
      * @return Rows and columns correspond to PointIds of the mesh. */
    def cotangentWeightMatrix(ref: TriangleMesh3D): CSCMatrix[Double] = {
      val n: Int = ref.pointSet.numberOfPoints
      //calculate all the cotangent weights
      val builderCOT = new CSCMatrix.Builder[Double](n, n)
      ref.triangulation.triangleIds.foreach(tid => {
        val tr = ref.triangulation.triangle(tid)
        val pta = ref.position.atPoint(tr.ptId1)
        val ptb = ref.position.atPoint(tr.ptId2)
        val ptc = ref.position.atPoint(tr.ptId3)
        val ab = (ptb - pta).normalize
        val bc = (ptc - ptb).normalize
        val ca = (pta - ptc).normalize
        val alpha = math.acos(ab.dot(-ca))
        val beta = math.acos(bc.dot(-ab))
        val gamma = math.acos(ca.dot(-bc))
        val cotA = 1.0 / math.tan(alpha)
        val cotB = 1.0 / math.tan(beta)
        val cotC = 1.0 / math.tan(gamma)
        builderCOT.add(tr.ptId1.id, tr.ptId2.id, cotC) //values are incremented if we add to same position in the matrix where we already have a value
        builderCOT.add(tr.ptId2.id, tr.ptId3.id, cotA)
        builderCOT.add(tr.ptId3.id, tr.ptId1.id, cotB)
      })
      val COT = builderCOT.result
      (COT + COT.t) *:* 2.0
    }
    val cot = cotangentWeightMatrix(ref)
    (i: PointId, j: PointId) => cot(i.id, j.id)
  }

  def unitWeight(i: PointId, j: PointId): Double = 1.0

  /** Constructs a weight function which is the heat kernel value k(x1,x2) = exp(- ||x1-x2||^2 / diffusionDistance^2 ) */
  def heatKernelWeights(ref: TriangleMesh3D, diffusionDistance: Double): LaplaceBeltramiWeightingFunction = {
    (i: PointId, j: PointId) => {
      val dist = (ref.position.atPoint(i) - ref.position.atPoint(j)).norm2
      math.exp(-dist / (diffusionDistance * diffusionDistance))
    }
  }
}
