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

import breeze.linalg.CSCMatrix

import scala.collection.mutable

/** permutation strategy for the Sparse Cholesky decomposition */
sealed trait PermutationStrategy {
  def findPermutation(A: CSCMatrix[Double]): Permutation
}

object PermutationStrategy {
  val default = NoPermutation
}

/** do not permute matrix entries */
case object NoPermutation extends PermutationStrategy {
  override def findPermutation(A: CSCMatrix[Double]): Permutation = Permutation.identity(A.cols)
}

/** Cuthill-McKee permutation finder */
case object CuthillMcKee extends PermutationStrategy {

  import CSCMatrixGraph._

  override def findPermutation(A: CSCMatrix[Double]): Permutation = {
    require(A.cols == A.rows, "matrix is not square")
    // breadth-first search with minimum degrees visited first
    val n = A.cols
    val ordering = new Array[Int](n)
    val allNodes: Set[Int] = (0 until n).toSet
    val visited = mutable.Set.empty[Int]
    val queue = mutable.Queue.empty[Int] // node
    var done = 0
    while (done < n) {
      // clean queue of visited nodes
      // get next node to visit
      val node: Int = {
        if (queue.nonEmpty) {
          // from queue
          queue.dequeue()
        } // as next minimum degree node
        else {
          val notVisited = allNodes.diff(visited)
          notVisited.map(n => (degree(n, A), n)).min._2
        }
      }
      // visit node:
      visited.add(node)
      ordering(done) = node
      done += 1
      // get all unvisited neighbours
      val newNodes = neighbours(node, A).toSet.diff(visited).diff(queue.toSet)
      // add to queue, in order of increasing degree
      newNodes.toIndexedSeq.sortBy(n => degree(n, A)).foreach(queue.enqueue(_))
      assert(newNodes.forall(n => !visited.contains(n)), "visited nodes added to queue")
      assert(queue.forall { n => !visited.contains(n) },
             s"visited nodes in queue, done=$done, q=$queue, visited=$visited"
      )
    }
    new Permutation(ordering)
  }
}

case object ReverseCuthillMcKee extends PermutationStrategy {
  override def findPermutation(A: CSCMatrix[Double]): Permutation = {
    val P = CuthillMcKee.findPermutation(A)
    new Permutation(P.toArray.reverse)
  }
}

/** look a t CSCMatrix as a graph (for permutation finders) */
object CSCMatrixGraph {
  def degree(n: Int, A: CSCMatrix[Double]): Int = {
    require(n >= 0 && n < A.cols, "invalid node number")
    val cStart = A.colPtrs(n)
    val cEnd = A.colPtrs(n + 1)
    cEnd - cStart - 1 // remove diagonal entry
  }

  def neighbours(n: Int, A: CSCMatrix[Double]): Array[Int] = {
    require(n >= 0 && n < A.cols, "invalid node number")
    val cStart = A.colPtrs(n)
    val cEnd = A.colPtrs(n + 1)
    A.rowIndices.slice(cStart, cEnd)
  }
}
