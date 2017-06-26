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

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import scalismo.numerics.PivotedCholesky.{AbsoluteTolerance, NumberOfEigenfunctions, RelativeTolerance, StoppingCriterion}

import scala.collection.mutable.ArrayBuffer

/** performs a Pivoted Cholesky decomposition */
object PivotedCholesky {
  /** get a pivoted Cholesky decomposition of matrix A (calculates strongest components, best for low-rank approximation) */
  def pivotedCholesky(A: DenseMatrix[Double], stoppingCriterion: StoppingCriterion): DenseMatrix[Double] = {
    require(A.rows == A.cols, "works only for square matrices")
    pivotedCholesky((i, j) => A(i, j), A.rows, stoppingCriterion)
  }

  /** get a pivoted Cholesky decomposition of function K(i,j) (calculates strongest components, best for low-rank approximation) */
  def pivotedCholesky(K: (Int, Int) => Double, dim: Int, stoppingCriterion: StoppingCriterion): DenseMatrix[Double] = {
    // assume A is positive definite and symmetric
    if (dim == 0) {
      return DenseMatrix.zeros[Double](0, 0)
    }

    val zeroTolerance = 1e-15
    val n = dim

    // extract diagonal
    val D = DenseVector.zeros[Double](n)
    for(i <- 0 until n) D(i) = K(i,i)
    def error(col: Int) = sum(D(col until D.length).map(math.abs))

    val (maxCols, errorThreshold) = stoppingCriterion match {
      case RelativeTolerance(relTol) => (dim, error(0) * relTol)
      case AbsoluteTolerance(eps) => (dim, eps)
      case NumberOfEigenfunctions(i) => (math.min(dim, i), zeroTolerance)
    }

    // start with empty factor and identity permutation
    val L = new PivotedCholeskyFactor(n, maxCols)
    val P = Array.tabulate(n)(i => i)

    def swapP(i: Int, j: Int) = {
      val t = P(i)
      P(i) = P(j)
      P(j) = t
    }

    // column j
    var col: Int = 0
    while(col < maxCols && error(col) > errorThreshold) {
      // find best pivot
      //      val piv = (col until n).map(i => (i, D(P(i)))).maxBy(_._2)._1
      // optimized pivot finder
      var piv = col
      var max = D(P(col))
      var i = col + 1
      while(i < n) {
        val v = D(P(i))
        if (v > max) {
          max = v
          piv = i
        }
        i += 1
      }
      // update permutation: swap current column with pivot
      swapP(piv, col)
      val pCol = P(col)
      // println(s"col=$col, error=${error(col)}, pivot=$piv, P(col)=$pCol")

      // calculate Schur complement, appropriately permuted
      // pivot element: diagonal with -L*L.t correction so far
      val pivEl = D(pCol)
      require(pivEl > zeroTolerance, s"Pivoted Cholesky factorization only works for positive semi-definite matrices, value too close to zero d=$pivEl")

      // explicit multiplication: find Schur complement, current column "col"
      val Lcol = new Array[Double](n)
      // diagonal element, sqrt and division, ensure numerical robustness wrt very small negatives
      val diagEl = math.sqrt(math.max(0.0, pivEl))
      Lcol(pCol) = diagEl
      // initialize factor column with elements from A
      for(row <- col + 1 until n) {
        val pRow = P(row)
        Lcol(pRow) += K(pRow, pCol)
      }
      // for each existing factor (column)
      var component = 0
      while(component < col) {
        // fast access to current factor
        val Lf = L.col(component)
        val Lfcol = Lf(pCol)
        // fill current column for rows from current column + 1 to end
        var row = col + 1
        while(row < n) {
          // find permuted row index
          val pRow = P(row)
          // find Schur complement (A - L*L.t)/diagEl
          // update current column of L: only L*L.t part
          Lcol(pRow) -= Lfcol * Lf(pRow)
          // next row
          row += 1
        }
        // next factor
        component += 1
      } // new cholesky column complete
      // update diagonal elements, remove contribution from this vector of the Cholesky factorization
      // also finalize current L column: division by diagEl is still missing
      var row = col + 1
      while(row < n) {
        val pRow = P(row)
        val l = Lcol(pRow)/diagEl
        Lcol(pRow) = l
        D(pRow) -= l*l
        row += 1
      }
      // completed column: append to L
      L.addCol(Lcol)
      // next column
      col += 1
    }
    // convert L data structure to proper matrix
    L.toDenseMatrix
  }
}

/** hold a growing Cholesky factor, allows fast addition of columns, direct random index access */
private class PivotedCholeskyFactor(d: Int, sizeHint: Int = 100) {
  val cols = new ArrayBuffer[Array[Double]](sizeHint)

  /** access element at L(i, j) */
  def apply(row: Int, col: Int): Double = {
    require(row >= 0 && row < d)
    require(col >= 0 && col < cols.length)
    cols(col)(row)
  }

  /** get column i as DenseVector[Double] */
  def col(i: Int): Array[Double] = cols(i)

  /** append column to end */
  def addCol(vec: Array[Double]): Unit = {
    require(vec.length == d)
    cols += vec
  }

  /** convert to DenseMatrix */
  def toDenseMatrix: DenseMatrix[Double] = {
    val m = DenseMatrix.zeros[Double](d, cols.size)
    for(i <- cols.indices) {
      m(::,i) := DenseVector(cols(i))
    }
    m
  }
}