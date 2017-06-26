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

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector, trace}
import org.scalatest.PrivateMethodTester
import scalismo.faces.FacesTestSuite
import scalismo.numerics.PivotedCholesky.{AbsoluteTolerance, NumberOfEigenfunctions, RelativeTolerance}

class PivotedCholeskyTests extends FacesTestSuite with PrivateMethodTester {

  val d = 20
  val cols = 10

  val gtMatrix = DenseMatrix.fill(d, cols)(rnd.scalaRandom.nextDouble())

  val tol = 1e-11

  describe("PivotedCholeskyFactor data structure:") {

    val fac = new PivotedCholeskyFactor(d, cols/5) // expect 3 reallocations

    it("can be filled with random numbers columns") {
      for (c <- 0 until cols) {
        val col: Array[Double] = gtMatrix(::, c).toDenseVector.toArray
        fac.addCol(col)
        // actual column should be identical
        fac.cols(c) shouldBe col
        // allows random access with proper values
        for (r <- 0 until d) {
          fac(r, c) shouldBe col(r)
        }
      }
    }

    it("converts to a DenseMatrix") {
      val m = fac.toDenseMatrix
      for (r <- 0 until d; c <- 0 until cols)
        m(r, c) shouldBe gtMatrix(r, c)
    }

    it("allows for random access") {
      for (r <- 0 until 10) {
        val row = rnd.scalaRandom.nextInt(d)
        val col = rnd.scalaRandom.nextInt(cols)
        fac(row, col) shouldBe gtMatrix(row, col)
      }
    }
  }

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

  describe("Pivoted Cholesky factorization") {
    // random matrices
    val A = randomDiagDomBandMatrix(d, 10).toDense
    val b = DenseVector.rand[Double](d)

    it("it should factorize a random positive definite matrix completely") {
      val L = PivotedCholesky.pivotedCholesky(A, NumberOfEigenfunctions(A.cols))
      val Am = L * L.t
      trace(A - Am) should be < 1e-10
    }

    it("it stops after reaching required accuracy (relative tolerance)") {
      val relativeTolerance = 0.25
      val L = PivotedCholesky.pivotedCholesky(A, RelativeTolerance(relativeTolerance))
      val Am = L * L.t
      trace(A - Am) should be <= (relativeTolerance * trace(A))
      L.cols should be < A.cols
    }

    it("it stops after reaching required accuracy (absolute tolerance)") {
      val absoluteTolerance = trace(A) * 0.5
      val L = PivotedCholesky.pivotedCholesky(A, AbsoluteTolerance(absoluteTolerance))
      val Am = L * L.t
      trace(A - Am) should be <= absoluteTolerance
      L.cols should be < A.cols
    }

    it("it stops after calculating enough factors") {
      val numberOfEigenfunctions = NumberOfEigenfunctions(10)
      val L = PivotedCholesky.pivotedCholesky(A, numberOfEigenfunctions)
      val Am = L * L.t
      trace(A - Am) should be <= trace(A)
      L.cols shouldBe numberOfEigenfunctions.n
    }

  }
}

