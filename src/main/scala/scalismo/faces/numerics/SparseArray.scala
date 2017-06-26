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

import breeze.linalg.SparseVector

/** sparse array, prevents boxing */
private[numerics] class SparseArray(var index: Array[Int], var data: Array[Double], var nnz: Int, val length: Int) {
  require(nnz <= length, "too many non zeros")
  require(data.length == index.length, "data and index have different length")
  require(data.length >= nnz, "data array is too short")

  def activeSize = nnz

  def update(i: Int, v: Double): Unit = {
    val offset = findOffset(i)
    if (offset >= 0)
      data(offset) = v
    else {
      val insert = ~offset
      nnz += 1
      if (nnz > index.length) reallocate()

      // insert
      // move right part
      System.arraycopy(index, insert, index, insert + 1, nnz - insert - 1)
      System.arraycopy(data, insert, data, insert + 1, nnz - insert - 1)
      // insert data
      index(insert) = i
      data(insert) = v
    }
  }

  private def reallocate() = {
    val newLength = math.max(nnz + 1, index.length * 2)
    val _index = new Array[Int](newLength)
    val _data = new Array[Double](newLength)
    System.arraycopy(index, 0, _index, 0, index.length)
    System.arraycopy(data, 0, _data, 0, data.length)
    index = _index
    data = _data
  }

  def apply(i: Int): Double = {
    val offset = findOffset(i)
    if (offset >= 0)
      data(offset)
    else
      0.0
  }

  def findOffset(i: Int): Int = util.Arrays.binarySearch(index, 0, nnz, i)

  def toDense: Array[Double] = {
    val arr = new Array[Double](length)
    var i = 0
    while (i < nnz) {
      val ind = index(i)
      arr(ind) = data(i)
      i += 1
    }
    arr
  }
}

object SparseArray {
  def apply(vector: SparseVector[Double]): SparseArray = {
    val nnz = vector.activeSize
    val index = new Array[Int](nnz)
    val data = new Array[Double](nnz)
    System.arraycopy(vector.index, 0, index, 0, nnz)
    System.arraycopy(vector.data, 0, data, 0, nnz)
    new SparseArray(index, data, nnz, vector.length)
  }
}
