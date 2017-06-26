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

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector, norm}
import scalismo.faces.FacesTestSuite

import scala.collection.mutable

class CholeskyTests extends FacesTestSuite {

  val n = 50
  val tol = 1e-11

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

  def randomPermutation(n: Int): Array[Int] = {
    // load id
    val p = (0 until n).toArray
    // swap random elements
    var i = n - 1
    while (i > 0) {
      val other = rnd.scalaRandom.nextInt(i + 1)
      // swap i and other
      val t = p(other)
      p(other) = p(i)
      p(i) = t
      i -= 1
    }
    p
  }

  def matNorm(A: DenseMatrix[Double]): Double = norm(A.toDenseVector)
  def matNorm(A: CSCMatrix[Double]): Double = norm(A.toDenseMatrix.toDenseVector)

  describe("Cholesky (Dense)") {
    it("decomposes the matrix A properly: A=L*L.t") {
      val L: DenseMatrix[Double] = DenseCholesky(Ad)
      val D: DenseMatrix[Double] = Ad - L * L.t
      matNorm(D) should be < tol
    }

    it("solves the system Ax=b") {
      val x = DenseCholesky.solve(Ad, b)
      norm(b - Ad * x) should be < tol
    }
  }

  describe("Sparse Cholesky") {
    it("decomposes the matrix A properly: A=L*L.t") {
      val L: CSCMatrix[Double] = SparseCholesky(A)
      matNorm(A - L * L.t) should be < tol
    }

    it("solves the system Ax=b") {
      val x = SparseCholesky.solve(A, b)
      norm(b - A * x) should be < tol
    }

    it("can permute a sparse matrix") {
      val perm = new Permutation(randomPermutation(n))
      val pA = Permutation.permuteSparseMatrix(A, perm)
      for (i <- 0 until pA.rows; j <- 0 until pA.cols) {
        pA(i, j) shouldBe A(perm(i), perm(j))
      }
    }

    it("can permute a sparse matrix without effect (id permutation)") {
      val permId = Permutation.identity(n)
      val pA = Permutation.permuteSparseMatrix(A, permId)
      pA shouldBe A
    }

    it("can solve a permuted system") {
      val perm = new Permutation(randomPermutation(n))
      val pA = Permutation.permuteSparseMatrix(A, perm)
      val L = SparseCholesky(pA)
      val x = SparseCholesky.substitutionSolver(L, b, perm)
      norm(b - A * x) should be < tol
    }

    it("can automatically find a permutation to reduce fill-in") {
      val P = ReverseCuthillMcKee.findPermutation(A)
      Permutation.isValid(P.toArray) shouldBe true

      val pA = Permutation.permuteSparseMatrix(A, P)

      val L = SparseCholesky(A)
      val pL = SparseCholesky(pA)

      pL.activeSize should be <= L.activeSize

      // solution check
      val x = SparseCholesky.substitutionSolver(L, b)
      val px = SparseCholesky.substitutionSolver(pL, b, P)

      norm(b - A * x) should be < tol
      norm(b - A * px) should be < tol
    }

    it("support the PermutationStrategy interface") {
      val x = SparseCholesky.solve(A, b, ReverseCuthillMcKee)
      norm(b - A * x) should be < tol
    }

  }

  def extractSparsityPattern(A: CSCMatrix[Double]): Set[(Int, Int)] = {
    val spPattern = mutable.Set.empty[(Int, Int)]
    for (col <- 0 until A.cols) {
      val cS = A.colPtrs(col)
      val cE = A.colPtrs(col + 1)
      for (e <- cS until cE) {
        val row = A.rowIndices(e)
        spPattern.add((row, col))
      }
    }
    spPattern.toSet
  }

  describe("An incomplete Cholesky decomposition of a random sparse matrix (diagonally dominant)") {
    val A = randomDiagDomMatrix(n, math.max(2, n / 100))
    val incL = SparseCholesky.incompleteSparseCholesky(A)

    it("has the same sparsity pattern as A") {
      val spA: Set[(Int, Int)] = extractSparsityPattern(A)
      val spL: Set[(Int, Int)] = extractSparsityPattern(incL)
      // reduce to lower triangular
      val spALT: Set[(Int, Int)] = spA.filter { case (row, col) => row >= col }
      spL shouldBe spALT
    }

    it("is not too different from A") {
      matNorm(A - incL * incL.t) should be < n * 0.1
    }
  }
}
