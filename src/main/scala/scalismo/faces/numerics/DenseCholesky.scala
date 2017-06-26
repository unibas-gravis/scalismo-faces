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

import breeze.linalg.{DenseMatrix, DenseVector}

/** Cholesky decomposition */
object DenseCholesky {
  /** perform a dense Cholesky factorization of matrix A, can only deal with 10k+ elements */
  def apply(A: DenseMatrix[Double]): DenseMatrix[Double] = cholesky(A)

  /** perform a dense Cholesky factorization of matrix A, can only deal with 10k+ elements */
  def cholesky(A: DenseMatrix[Double], tolerance: Double = 1e-15): DenseMatrix[Double] = {
    // assume A is positive definite and symmetric
    require(A.rows == A.cols, "A needs to be symmetric, at least quadratic")
    if (A.rows == 0) {
      return A
    }
    val n = A.rows
    // start with empty factor
    val L = DenseMatrix.zeros[Double](A.rows, A.cols)
    // column 0
    val S = A(::, 0)
    if (S(0) < -tolerance) throw new Exception(s"Cholesky factorization only works with positive definite matrices, negative value encountered: ${S(0)}")
    val d = math.sqrt(S(0))
    L(0, 0) = d
    L(1 until n, 0) := S(1 to -1) / d
    // column j: 1 until n-1 (w/o first and last)
    var j = 1
    while (j < n - 1) {
      val Lr = L(j until n, 0 until j)
      val L1 = Lr(0, 0 until j).t
      val S = A(j until n, j) - Lr * L1
      val d = math.sqrt(S(0))
      L(j, j) = d
      L(j + 1 until n, j) := S(1 to -1) / d
      j += 1
    }
    // last entry
    val Lr = L(n - 1, ::)
    L(n - 1, n - 1) = math.sqrt(A(n - 1, n - 1) - Lr * Lr.t)
    // finished, factor is filled
    L
  }

  /** solve the linear system Ax = b for positive definite and symmetric matrix A using a Cholesky decomposition */
  def solve(A: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
    val L = cholesky(A)
    substitutionSolver(L, b)
  }

  /**
   * solves LL.t * x = b, for L dense lower triangular matrix,
   *
   * @param choleskyFactor Cholesky factor of A
   */
  def substitutionSolver(choleskyFactor: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
    require(choleskyFactor.rows == b.length, "dimensions disagree")
    require(choleskyFactor.rows == choleskyFactor.cols, "L must be square")

    val L = choleskyFactor
    val n = L.rows

    // solve L Lt x = b
    // 1) solve Ly = b
    val y = DenseVector.zeros[Double](n)
    // for each row substitute
    var row = 0
    while (row < n) {
      // previous elements
      val sum = L(row, 0 until row) * y(0 until row)
      // divide by diagonal element
      y(row) = (b(row) - sum) / L(row, row)
      row += 1
    }

    // 2) solve Lt x = y
    val x = DenseVector.zeros[Double](n)
    row = n - 1 // bwd substitution from right to left
    while (row >= 0) {
      // previous elements
      val sum = L.t(row, row + 1 until n) * x(row + 1 until n)
      // divie by diagonal element
      x(row) = (y(row) - sum) / L(row, row)
      row -= 1
    }
    // x contains result
    x
  }
}
