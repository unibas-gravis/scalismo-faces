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

import breeze.linalg.{CSCMatrix, DenseVector, VectorBuilder}

/** permutation */
class Permutation(p: Array[Int]) {
  val length: Int = p.length

  protected val invP: Array[Int] = {
    val arr = new Array[Int](p.length)
    var i = 0
    while (i < p.length) {
      arr(p(i)) = i
      i += 1
    }
    arr
  }

  def apply(i: Int): Int = p(i)

  /** apply inverse permutation */
  def inverse(i: Int): Int = invP(i)

  /** get inverted permutation */
  def inverted: Permutation = new Permutation(invP) {
    override val invP: Array[Int] = p
  }

  def toArray: Array[Int] = p.clone()

  override def toString: String = p.toIndexedSeq.toString
}

object Permutation {

  /** create default identity permutation */
  def identity(n: Int): Permutation = new Permutation(Array.empty[Int]) {
    override def apply(i: Int): Int = i

    override def inverse(i: Int): Int = i

    override val length: Int = n

    override def inverted = this

    override def toArray: Array[Int] = (0 until n).toArray
  }

  /** checks whether a permutation is valid */
  def isValid(p: Array[Int]): Boolean = {
    val c = new Array[Boolean](p.length)
    p.forall(i => {
      val v = c(i)
      c(i) = true
      !v
    })
  }

  /** reorder a vector, P gives new order */
  def permuteVector(v: DenseVector[Double], P: Permutation): DenseVector[Double] = {
    assert(v.length == P.length, "permutation length is wrong")
    val w = DenseVector.zeros[Double](v.length)
    var i = 0
    while (i < w.length) {
      w(i) = v(P(i))
      i += 1
    }
    w
  }

  /** symmetrically permutes a matrix: A' = PAP.t, P holds new ordering */
  def permuteSparseMatrix(A: CSCMatrix[Double], P: Permutation): CSCMatrix[Double] = {
    // permutation encoding:
    // build inverse permutation (for row index updates)
    assert(P.length == A.cols && P.length == A.rows, "permutation has wrong size")
    // new matrix storage
    val pData = new Array[Double](A.data.length)
    val pColPtrs = new Array[Int](A.colPtrs.length)
    val pRowIndices = new Array[Int](A.rowIndices.length)
    assert(pRowIndices.length == pData.length, "data must be of the same length as rowIndices")
    assert(pColPtrs.length == A.cols + 1, "colPtrs must be of size A.cols+1")
    // column permutation is easy: single traversal of CSCMatrix structure
    pColPtrs(0) = 0
    var col = 0
    while (col < A.cols) {
      // find old column in new ordering
      val colInA = P(col)
      // start and stop in A's data and row indices structure
      val cStart = A.colPtrs(colInA)
      val cEnd = A.colPtrs(colInA + 1)
      val elemInCol = cEnd - cStart
      // set length of new column
      val pStart = pColPtrs(col)
      val pEnd = pStart + elemInCol
      pColPtrs(col + 1) = pEnd
      // update row indices
      val colBuilder = new VectorBuilder[Double](A.rows)
      var r = 0
      while (r < elemInCol) {
        // old row index of element
        val rowInA = A.rowIndices(cStart + r)
        // row after permutation
        val pRow = P.inverse(rowInA)
        // add element to vector builder at new row position
        colBuilder.add(pRow, A.data(cStart + r))
        r += 1
      }
      // build column vector
      val colVec = colBuilder.toSparseVector
      assert(elemInCol == colVec.activeSize, "column vector has wrong number of elements")
      System.arraycopy(colVec.index, 0, pRowIndices, pStart, elemInCol)
      System.arraycopy(colVec.data, 0, pData, pStart, elemInCol)
      // next column
      col += 1
    }
    // construct CSCMatrix
    new CSCMatrix[Double](pData, A.rows, A.cols, pColPtrs, pRowIndices)
  }

}
