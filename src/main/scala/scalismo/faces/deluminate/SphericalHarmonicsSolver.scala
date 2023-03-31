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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.color.RGB
import scalismo.faces.numerics.SphericalHarmonics
import scalismo.geometry.{_3D, EuclideanVector}

/** provides functionality to solve the linear illumination equations for a given geometry and resulting radiance */
object SphericalHarmonicsSolver {

  case class IlluminatedPoint(normal: EuclideanVector[_3D], radiance: RGB, albedo: RGB)

  /** solve linear illumination equations for an unknown light field L_l: radiance = albedo * sum_l Y_l(normal) * L_l */
  def solveSHSystem(points: IndexedSeq[IlluminatedPoint], nBands: Int): IndexedSeq[EuclideanVector[_3D]] = {
    // prepare SH basis
    val nSH = SphericalHarmonics.totalCoefficients(nBands)
    val kernel = IndexedSeq.fill(nSH)(1.0)
    solveSHSystemDeconvolve(points, kernel)
  }

  /**
   * solve linear illumination equations for an unknown light field L_l, deconvolve a kernel k_l: radiance = albedo *
   * sum_l Y_l(normal) * L_l * k_l
   * @param points
   *   Illuminated points with normal, radiance and albedo
   * @param kernel
   *   Kernel on SH coefficients, usually Lambert kernel for diffuse reflectance, determines number of SH coefficients
   *   returned
   */
  def solveSHSystemDeconvolve(points: IndexedSeq[IlluminatedPoint],
                              kernel: IndexedSeq[Double]
  ): IndexedSeq[EuclideanVector[_3D]] = {
    require(points.nonEmpty)
    // direct access data
    val radiances = points.map(_.radiance)
    val normals = points.map(_.normal)
    val albedi = points.map(_.albedo)

    // prepare SH basis
    val nSH = kernel.length
    val shBasis = IndexedSeq.tabulate(nSH)(i => SphericalHarmonics.shBasisFunction(i))

    // build target vector on rhs: b (3*#points x 1), vectorize all colors to r, g, b
    val b = DenseVector(radiances.toArray.flatMap(r => r.toVector.toArray))

    // build matrix: (3*#points) x  (3*#lightCoefficients)
    def matrixBuilder(i: Int, j: Int): Double = {
      // major indices: point, light coefficient
      val pointIndex = i / 3
      val shCoeffIndex = j / 3
      // minor indices: color index R, G, B
      val pointColorIndex = i % 3
      val shColorIndex = j % 3
      // matrix element: albedo[point, color] * Y[shCoeff](normal[point]) * kernel(shCoeff) * delta(pointColor, shColor)
      if (pointColorIndex == shColorIndex)
        albedi(pointIndex).toVector.toArray(pointColorIndex) * shBasis(shCoeffIndex)(normals(pointIndex)) * kernel(
          shCoeffIndex
        )
      else
        0.0
    }
    val A: DenseMatrix[Double] = DenseMatrix.tabulate(3 * points.length, 3 * nSH)(matrixBuilder)
    // solve linear system
    val lightField: DenseVector[Double] = A \ b

    // extract channeled coefficients
    val shCoeffs: IndexedSeq[EuclideanVector[_3D]] =
      lightField.toArray.grouped(3).map(a => EuclideanVector[_3D](a)).toIndexedSeq

    // finished
    shCoeffs
  }
}
