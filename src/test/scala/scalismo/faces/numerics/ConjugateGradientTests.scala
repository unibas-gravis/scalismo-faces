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

import breeze.linalg.{norm, CSCMatrix, DenseVector}
import scalismo.faces.FacesTestSuite

class ConjugateGradientTests extends FacesTestSuite {

  val n = 50
  val tol = 1e-10

  val A = randomDiagDomBandMatrix(n, 10)
  val Ad = A.toDense
  val b = DenseVector.rand[Double](n)

  /** build a random PSD matrix: band matrix, diagonally dominant */
  def randomDiagDomBandMatrix(n: Int, band: Int): CSCMatrix[Double] = {
    val builder = new CSCMatrix.Builder[Double](n, n)
    // generate band
    for (i <- 0 until n; j <- i + 1 until math.min(n, i + band)) {
      val e = rnd.scalaRandom.nextDouble()
      builder.add(i, j, e)
      builder.add(j, i, e)
    }
    // diagonally dominant
    for (i <- 0 until n) {
      builder.add(i, i, band)
    }
    builder.result()
  }

  /** build a random PSD matrix: sparse matrix, diagonally dominant */
  def randomDiagDomMatrix(n: Int, elementsPerColumn: Int): CSCMatrix[Double] = {
    val builder = new CSCMatrix.Builder[Double](n, n)
    // generate band
    for (col <- 0 until n; j <- 0 until elementsPerColumn) {
      val e = rnd.scalaRandom.nextDouble()
      val row = rnd.scalaRandom.nextInt(n)
      builder.add(row, col, e)
      builder.add(col, row, e)
    }
    // diagonally dominant
    for (i <- 0 until n) {
      builder.add(i, i, 2 * elementsPerColumn)
    }
    builder.result()
  }

  describe("ConjugateGradient solver") {
    it("can solve a random sparse linear system to high accuracy") {
      val x = ConjugateGradient.solveSparse(A, b, tol / 10)
      norm(b - A * x) should be < tol
    }
  }

  describe("PreconditionedConjugateGradient solver") {
    it("can solve a random sparse linear system to high accuracy with the incomplete Cholesky preconditioner") {
      val M = PreconditionedConjugateGradient.incompleteCholeskyPreconditioner(A)
      val x = PreconditionedConjugateGradient.solveSparse(A, b, M, tol / 10)
      norm(b - A * x) should be < tol
    }
  }
}
