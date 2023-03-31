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

import java.util

import breeze.linalg._
import scalismo.faces.image._

/** sparse Cholesky decomposition */
object SparseCholesky {

  /** calculate sparse Cholesky decompositon, return factor L for A = LL^T ^ */
  def apply(A: CSCMatrix[Double]): CSCMatrix[Double] = sparseCholesky(A)

  /**
   * solve the linear system Ax = b for a sparse positive definite and symmetric matrix A using a Cholesky decomposition
   */
  def solve(A: CSCMatrix[Double],
            b: DenseVector[Double],
            fillInReduction: PermutationStrategy = PermutationStrategy.default
  ): DenseVector[Double] = {
    val P = fillInReduction.findPermutation(A)
    val pA = Permutation.permuteSparseMatrix(A, P)
    val L = sparseCholesky(pA)
    substitutionSolver(L, b, P)
  }

  /** Cholesky decomposition of a sparse, positive definite and symmetric matrix A */
  def sparseCholesky(A: CSCMatrix[Double], tolerance: Double = 1e-15): CSCMatrix[Double] = {
    // assume A is positive definite and symmetric
    require(A.rows == A.cols, "A needs to be symmetric, at least quadratic")
    val n = A.rows
    // start with empty factor
    val L = new SparseCholeskyFactor(n)
    // special column accumulator for fast write access and fast conversion to SparseVector
    val columnBuilder = new SparseAccumulator(n)
    // bandwidth protection: sparsity is limited by bandwidth in both A and L
    var bandWidth = 0
    // column j: 0 until n (no special treatment of first and last col)
    var j = 0
    while (j < n) {
      // initialize sparse accumulator with column of A (below diagonal)
      columnBuilder.initWithColumn(A, j)
      bandWidth = min(n - 1, max(bandWidth, columnBuilder.lastIndex)) // last row with non-zero elements in A
      // traverse row j in L(j, k=0 until j), for each non-zero subtract L(j, k)*L(j until n, k)
      // use cursors structure to keep track of current row entries in columns
      // along row j from left to diagonal
      val Lrow = L.rows(j)
      if (Lrow != null) {
        // use sparse rows structure: e active element, next non-zero element
        var e = 0
        while (e < Lrow.activeSize) {
          val k = Lrow.index(e)
          val l = Lrow.data(e)

          val c = L.cursors(k)
          val Lcol = L.cols(k)
          // subtract from acc: columnBuilder -= L(j,k) * L(j:n,k).t
          // subtract: acting on VectorBuilder, subtract col L(j to n,k)
          var actEl = math.max(c, 0)
          while (actEl < Lcol.activeSize) {
            // subtract element
            val row = Lcol.index(actEl)
            if (row <= bandWidth) {
              // add elements only within bandwidth, outside is zero (cancels with later elements)
              val data = Lcol.data(actEl)
              columnBuilder.add(row, -data * l)
            }
            // next element to subtract
            actEl += 1
          }
          // move cursor
          L.cursors(k) += 1
          // next non-zero col in row
          e += 1
        }
      }
      // work with SparseVector now, sparsity pattern is stable
      val S = columnBuilder.toSparseVector
      // sqrt and division of acc
      assert(S.activeSize > 0 && S.index(0) == j, "first element is not diagonal entry") // first element on diagonal
      assert(S.data(0) > -tolerance,
             s"Cholesky decomposition needs positive definite matrix! Negative value encountered: v=${S.data(0)}"
      )
      // negative number within numerical tolerance, set to zero to make sqrt work
      if (S.data(0) < 0.0) S.data(0) = 0.0
      // sqrt of first element
      val d = math.sqrt(S.data(0))
      S.data(0) = d
      // divide all lower elements by d
      var i = 1
      while (i < S.activeSize) {
        S.data(i) /= d
        // next active element in S
        i += 1
      }
      // update column value to new accumulator value
      L.setCol(j, S)
      // next column
      j += 1
    }
    // finished, factor is filled
    L.toCSCMatrix
  }

  /**
   * solves LL.t * x = b, for L sparse lower triangular matrix,
   *
   * @param choleskyFactor
   *   Cholesky factor of A
   */
  def substitutionSolver(choleskyFactor: CSCMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
    require(choleskyFactor.rows == b.length, "dimensions disagree")
    require(choleskyFactor.rows == choleskyFactor.cols, "L must be square")

    val y = fwdSubstitution(choleskyFactor, b)
    bwdSubstitution(choleskyFactor, y)
  }

  /**
   * solves PLL.tP.t * x = b, for L sparse lower triangular matrix, P permutation
   *
   * @param choleskyFactor
   *   Cholesky factor of A
   */
  def substitutionSolver(choleskyFactor: CSCMatrix[Double],
                         b: DenseVector[Double],
                         P: Permutation
  ): DenseVector[Double] = {
    require(choleskyFactor.rows == b.length, "dimensions disagree")
    require(choleskyFactor.rows == choleskyFactor.cols, "L must be square")

    val pb = Permutation.permuteVector(b, P)
    val py = fwdSubstitution(choleskyFactor, pb)
    val px = bwdSubstitution(choleskyFactor, py)
    Permutation.permuteVector(px, P.inverted)
  }

  /** solve Lx = b with L lower triangular sparse matrix */
  private def fwdSubstitution(L: CSCMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
    require(L.cols == b.length, "dimensions disagree")
    require(L.cols == L.rows, "L should be quadratic")
    // solve L x = b, L lower triangular
    // target vector
    val n = b.length
    val x: DenseVector[Double] = b.copy
    // for each column: substitution from top to bottom
    var col = 0
    while (col < n) {
      // extract column slice
      val colStart = L.colPtrs(col)
      val colEnd = L.colPtrs(col + 1)
      // we need at least the diagonal element
      if (colEnd > colStart && L.rowIndices(colStart) == col) {
        // calculate first element of column y(col)
        x(col) /= L.data(colStart)
        val yc = x(col)
        // substitute for each nnz index
        var i = colStart + 1
        while (i < colEnd) {
          val d = L.data(i)
          val row = L.rowIndices(i)
          x(row) -= d * yc
          i += 1
        }
      } else {
        throw new Exception(
          "fwd substitution solver: diagonal element is not first in column! L needs to be lower triangular with non-zero diagonal"
        )
      }
      col += 1
    }
    x
  }

  /** solve L.t x = b with L lower triangular sparse matrix */
  private def bwdSubstitution(L: CSCMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
    require(L.cols == b.length, "dimensions disagree")
    require(L.cols == L.rows, "L should be quadratic")
    // solve L.t x = b, L lower triangular
    val n = b.length
    val x = b.copy
    var col = n - 1 // bwd substitution from right to left
    while (col >= 0) {
      // extract column slice
      val colStart = L.colPtrs(col)
      val colEnd = L.colPtrs(col + 1)
      // we need at least the diagonal element of the column
      if (colEnd > colStart && L.rowIndices(colStart) == col) {
        // calculate L(col+1 until n, col) * x(col+1 until n)
        var sum = 0.0
        var i = colStart + 1 // w/o diagonal element
        while (i < colEnd) {
          sum += L.data(i) * x(L.rowIndices(i))
          i += 1
        }
        // update x(col) and divide by diagonal element
        x(col) -= sum
        x(col) /= L.data(colStart)
      } else {
        throw new Exception(
          "bwd substitution solver: diagonal element is not first in column! L needs to be lower triangular with non-zero diagonal"
        )
      }
      col -= 1
    }
    // x contains result
    x
  }

  /**
   * calculate the incomplete Cholesky factorization, useful as a preconditioner for CG (only approximate, as sparse as
   * A, fast)
   */
  def incompleteSparseCholesky(A: CSCMatrix[Double], tolerance: Double = 1e-15): CSCMatrix[Double] = {
    // assume A is positive definite and symmetric
    require(A.rows == A.cols, "A is not square")
    val n = A.rows
    // start with empty factor
    val L = new SparseCholeskyFactor(n)
    // column j: 0 until n (no special treatment of first and last col)
    var j = 0
    while (j < n) {
      // initialize sparse accumulator with column of A (below diagonal)
      val currentColumn = extractColumnBelowDiagonal(A, j)

      // traverse row j in L(j, k=0 until j), for each non-zero subtract L(j, k)*L(j until n, k) from currentColumn -- do only where is non-zero ("incomplete")
      // use cursors structure to keep track of current row entries in columns
      // along row j from left to diagonal
      val Lrow = L.rows(j)
      if (Lrow != null) {
        // use sparse rows structure: e active element, next non-zero element
        var e = 0
        while (e < Lrow.activeSize) {
          val k = Lrow.index(e)
          val l = Lrow.data(e)
          // corresponding column in L
          val Lcol = L.cols(k)
          // traverse current column, do operation only for non-zero entries (restrict incomplete to sparsity pattern of A)
          var colEl = 0
          while (colEl < currentColumn.activeSize) {
            val row = currentColumn.index(colEl)
            // subtract from current column
            currentColumn.data(colEl) -= Lcol(row) * l
            // next column element
            colEl += 1
          }
          // next element in row L(j, k=0 until j)
          e += 1
        }
      }
      // work with SparseVector now, sparsity pattern is stable
      val S = currentColumn
      // sqrt and division of acc
      assert(S.activeSize > 0 && S.index(0) == j, "first element is not diagonal entry") // first element on diagonal
      assert(
        S.data(0) > -tolerance,
        s"(incomplete) Cholesky decomposition needs positive definite matrix! Negative value encountered: v=${S.data(0)}"
      )
      // negative number within numerical tolerance, set to zero to make sqrt work
      if (S.data(0) < 0.0) S.data(0) = 0.0
      // sqrt of first element
      val d = math.sqrt(S.data(0))
      S.data(0) = d
      // divide all lower elements by d
      var i = 1
      while (i < S.activeSize) {
        S.data(i) /= d
        // next active element in S
        i += 1
      }
      // update column value to new accumulator value
      L.setCol(j, S)
      // next column
      j += 1
    }
    // finished, factor is filled
    L.toCSCMatrix
  }

  private def extractColumnBelowDiagonal(A: CSCMatrix[Double], col: Int): SparseVector[Double] = {
    val cStart = A.colPtrs(col)
    val cEnd = A.colPtrs(col + 1)
    // find diagonal
    val diag = util.Arrays.binarySearch(A.rowIndices, cStart, cEnd, col)
    if (diag < 0) throw new Exception("diagonal not found, is required")
    val nnz = cEnd - diag
    val index = new Array[Int](nnz)
    val data = new Array[Double](nnz)
    System.arraycopy(A.data, diag, data, 0, nnz)
    System.arraycopy(A.rowIndices, diag, index, 0, nnz)
    new SparseVector[Double](index, data, A.rows)
  }

  /**
   * structure to build a sparse cholesky factor, efficient columns and rows, ~ CSC & CSR matrix, internal use only, no
   * safe bookkeeping
   */
  private class SparseCholeskyFactor(val length: Int) {
    val cols = new Array[SparseVector[Double]](length)
    // val rows = new Array[SparseVector[Double]](length)
    val rows = new Array[SparseArray](length)

    // active row cursors for faster lookup
    val cursors: Array[Int] = Array.fill(length)(-1)
    var nnz = 0

    def activeSize: Int = nnz

    def setCol(col: Int, vector: SparseVector[Double]): Unit = {
      require(vector.length == length)
      nnz += vector.activeSize
      cols(col) = vector
      cursors(col) = 1 // first element is diagonal, set cursor for next row to next element
      // update rows ... not optimal
      // need at least the same row
      var e = 0
      while (e < vector.activeSize) {
        val row = vector.index(e)
        if (rows(row) != null)
          rows(row)(col) = vector.data(e)
        else
          rows(row) = new SparseArray(Array(col, 0, 0, 0), Array(vector.data(e), 0.0, 0.0, 0.0), 1, length)
        // rows(row) = new SparseVector[Double](Array(col, 0, 0, 0), Array(vector.data(e), 0.0, 0.0, 0.0), 1, length)
        // next element
        e += 1
      }
    }

    /** build CSCMatrix structure (copy) */
    def toCSCMatrix: CSCMatrix[Double] = {
      val n = nnz
      val data = new Array[Double](n)
      val rowIndices = new Array[Int](n)
      val colPtrs = new Array[Int](length + 1)
      // fill structure
      // linear master index
      var i = 0
      var col = 0
      while (col < length) {
        assert(i < n, "master index got too large")
        // set column pointer to start
        colPtrs(col) = i
        // insert data and rowindex
        Array.copy(cols(col).data, 0, data, i, cols(col).activeSize)
        Array.copy(cols(col).index, 0, rowIndices, i, cols(col).activeSize)
        // advance master index
        i += cols(col).activeSize
        // next column
        col += 1
      }
      assert(i == n, "something went wrong with the number of elements")
      // set sentry: last coloumn pointer outside the array
      colPtrs(col) = n

      new CSCMatrix[Double](data, length, length, colPtrs, rowIndices)
    }
  }

  /** accumulator for cholesky factor creation, handles a sparse column with dense storage for fast access */
  private class SparseAccumulator(val length: Int) {
    // full structures for data and exists, fast direct access
    private val data = new Array[Double](length)
    private val exists = new Array[Boolean](length)
    // row index structure for fast conversion to sparse format
    private var index = new Array[Int](length)
    private var nnz: Int = 0

    def lastIndex: Int = index(nnz - 1)

    /** create a SparseVector (copy) */
    def toSparseVector: SparseVector[Double] = {
      // sort index array (index is unsorted!!)
      util.Arrays.sort(index, 0, nnz)
      // create sparse vector by extracting data
      val dt = new Array[Double](nnz)
      val in = new Array[Int](nnz)
      val length = this.length
      var e = 0
      while (e < nnz) {
        val r = index(e) // row
        dt(e) = data(r)
        in(e) = r
        e += 1
      }
      new SparseVector[Double](in, dt, length)
    }

    def add(i: Int, v: Double): Unit = {
      // require(i >= 0 && i < length, s"row is out of bounds, r=$i")
      // assert(nnz <= length, "too many nnz elements")
      if (exists(i)) {
        data(i) += v
      } else {
        data(i) = v
        exists(i) = true
        index(nnz) = i
        nnz += 1
      }
    }

    /** copy column of matrix to accumulator, replaces all former data */
    def initWithColumn(A: CSCMatrix[Double], j: Int): Unit = {
      clear()
      // copy over column of A
      val colStart = A.colPtrs(j)
      val colEnd = A.colPtrs(j + 1)
      // find diagonal element
      val diag = util.Arrays.binarySearch(A.rowIndices, colStart, colEnd, j)
      if (diag < 0) throw new Exception("cannot extract subdiagonal column elements, no diagonal found")
      // extract data below diagonal, including diagonal
      var k = diag
      while (k < colEnd) {
        val v = A.data(k)
        val r = A.rowIndices(k)

        data(r) = v
        exists(r) = true
        index(nnz) = r
        nnz += 1
        k += 1
      }
      assert(nnz == colEnd - diag, "nnz is wrong")
    }

    def clear(): Unit = {
      // reset structure
      var i = 0
      while (i < nnz) {
        val r = index(i)
        exists(r) = false
        i += 1
      }
      nnz = 0
    }
  }
}
