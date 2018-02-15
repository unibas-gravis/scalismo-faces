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

package scalismo.faces.numerics

import breeze.linalg.{DenseMatrix, DenseVector, diag}
import scalismo.common.PointId
import scalismo.faces.numerics.ArnoldiSymmetricEigenSolver.EigenvaluesFirst
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.{SurfacePointProperty, TriangleList}

/** MDS: Preserves predefined distances between points in a lower dimensional manifold in the least-squares sense.
  * According to: Zigelman, Gil, Ron Kimmel, and Nahum Kiryati. "Texture mapping using surface flattening via multidimensional scaling." */

object MultiDimensionalScaling {

  trait MDSWeightFunction extends ((PointId, PointId)=>Double)

  /** Calculates distance preserving mapping to a lower dimensional space in the least squares sense.
    * Distance between two points - each represented by an Int - has to be a valid distance measure.
    *
    * @param distance function that measures the distance between two points.
    * @param problemSize number of input points / size of output matrix
    * @return matrix problemSizexproblemSize
    */
  def apply(distance: (Int, Int) => Double, problemSize: Int, numberOfDimensionsToKeep: Int): DenseMatrix[Double] = {
    val W = DenseMatrix.zeros[Double](problemSize, problemSize)
    for (i <- 0 until problemSize) {
      for (j <- 0 until i) {
        val d = distance(i, j)
        W(i, j) = d
        W(j, i) = d
      }
    }
    //"convert" distance to similarity
    val J = DenseMatrix.eye[Double](problemSize) - (DenseMatrix.ones[Double](problemSize, problemSize) / problemSize.toDouble)
    val B = (J * W * J) *:* -0.5
    val C = (0.5 * B + 0.5 * B.t).toDenseMatrix

    val (eigenvalues, eigenvectors) = ArnoldiSymmetricEigenSolver.symmetricEigs(v => C * v, C.cols, numberOfDimensionsToKeep, EigenvaluesFirst.Largest, 1e-10, C.cols * 4)
    val sortedEigenvalues = eigenvalues.toArray.toIndexedSeq.zipWithIndex.sortBy(l=>l._1).reverse
    val D: DenseMatrix[Double] = diag(DenseVector(sortedEigenvalues.map(l => math.sqrt(math.abs(l._1))).toArray)).toDenseMatrix
    eigenvectors(::, sortedEigenvalues.map(_._2)).toDenseMatrix * D
  }

  /**
    * Computes a mesh parametrization according to the Multi Dimensional Scaling Scheme.
    * Distances can be supplied via the vertexDistance function. They are preserved in the resulting space.
    * @param triangulation
    * @param vertexDistance
    * @return
    */
  def parametrizationAsProperty(triangulation: TriangleList, vertexDistance: MDSWeightFunction): SurfacePointProperty[Point[_3D]] = {
    val n = triangulation.pointIds.length
    def dist(i: Int, j: Int) = vertexDistance(PointId(i), PointId(j))
    val mds: DenseMatrix[Double] = MultiDimensionalScaling(dist, n, 3)
    val embedding = IndexedSeq(mds(::, 0),
      mds(::, 1),
      mds(::, 2))
    val embeddedValues = (0 until mds.rows).map(i => Point(embedding(0)(i).toFloat, embedding(1)(i), embedding(2)(i)))
    SurfacePointProperty(triangulation, embeddedValues)
  }

}
