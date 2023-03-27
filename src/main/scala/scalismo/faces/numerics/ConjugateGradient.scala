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

import breeze.linalg.{CSCMatrix, DenseVector, norm}

/** Conjugate Gradient solver for sparse, large linear system */
object ConjugateGradient {

  /** solve system A*x = b, for a sparse matrix A using the Conjugate Gradient algorithm
    * uses initial guess of zero */
  def solveSparse(A: CSCMatrix[Double], b: DenseVector[Double], tolerance: Double = 1e-10): DenseVector[Double] = {
    // without initial guess: zero
    val initial = DenseVector.zeros[Double](b.length)
    solveSparse(A, b, initial, tolerance, 2 * b.length)
  }

  /** solve system A*x = b, for a sparse matrix A using the Conjugate Gradient algorithm */
  def solveSparse(A: CSCMatrix[Double], b: DenseVector[Double], xInit: DenseVector[Double], tolerance: Double, maxIter: Int): DenseVector[Double] = {
    require(A.cols == b.length, "matrix and vector dimensions disagree")
    // prepare
    val initial = initialState(A, b, xInit)
    val solver = cgIterator(A, b, initial).take(maxIter).dropWhile(state => norm(state.r) > tolerance).take(1)
    val solution = if (solver.hasNext) solver.next() else initial
    solution.x
  }

  /** create an initial CG step (use for cgIterator) */
  def initialState(A: CSCMatrix[Double], b: DenseVector[Double], x: DenseVector[Double]): CGState = {
    require(A.cols == b.length, "matrix and vector dimensions disagree")
    val r = b - A * x
    val p = r.copy
    CGState(x, r, p)
  }

  /** create an iterator of Conjugate Gradient steps */
  def cgIterator(A: CSCMatrix[Double], b: DenseVector[Double], initial: CGState): Iterator[CGState] = {
    Iterator.iterate(initial)(cgIteration(A, b, _))
  }

  /** a single Fletcher-Reeves Conjugate Gradient iteration (matrix A should not change between iterations) */
  def cgIteration(A: CSCMatrix[Double], b: DenseVector[Double], state: CGState): CGState = {
    val Ap: DenseVector[Double] = A * state.p
    val rsq: Double = state.r.t * state.r
    // alpha: step length
    val alpha: Double = rsq / (state.p.t * Ap)
    // update solution
    val x: DenseVector[Double] = state.x + state.p * alpha
    val r: DenseVector[Double] = state.r - Ap * alpha
    // conjugate direction update
    val beta: Double = (r.t * r) / rsq
    val p: DenseVector[Double] = r + state.p * beta
    CGState(x, r, p)
  }

  /** CG algorithm state (use for iterative interface) */
  case class CGState(x: DenseVector[Double], r: DenseVector[Double], p: DenseVector[Double])
}

