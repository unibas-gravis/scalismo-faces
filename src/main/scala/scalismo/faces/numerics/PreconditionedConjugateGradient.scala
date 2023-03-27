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

/** implements the preconditioned conjugate gradient for faster convergence with good preconditioners */
object PreconditionedConjugateGradient {

  /** solve system A*x = b, with sparse matrix A using the Preconditioned Conjuate Gradient algorithm */
  def solveSparse(A: CSCMatrix[Double], b: DenseVector[Double], preconditioner: DenseVector[Double] => DenseVector[Double], tolerance: Double = 1e-10): DenseVector[Double] = {
    val xInit = DenseVector.zeros[Double](b.length)
    solveSparse(A, b, xInit, preconditioner, tolerance, 2 * b.length)
  }

  /**
    * solve sparse linear system with the preconditioned conjugate gradient algorithm
    *
    * @param preconditioner solves Mx = b (x = preconditionSolver(b)), more efficient inv(M)*b (where is a crude but accessible approximation to A)
    */
  def solveSparse(A: CSCMatrix[Double],
                  b: DenseVector[Double],
                  xInit: DenseVector[Double],
                  preconditioner: DenseVector[Double] => DenseVector[Double],
                  tolerance: Double,
                  maxIter: Int): DenseVector[Double] = {
    require(A.cols == b.length, "matrix and vector dimensions disagree")
    // prepare
    // count iterations
    var i = 0
    // initial guess
    val initial = initialState(A, b, xInit, preconditioner)
    // solving iterator with counter and convergence criteria (maxIter or below tolerance)
    val solver = pcgIterator(A, b, preconditioner, initial).take(maxIter).dropWhile(state => norm(state.r) > tolerance).take(1)
    val solution = if (solver.hasNext) solver.next() else initial
    solution.x
  }

  /** create an initial state with an initial guess */
  def initialState(A: CSCMatrix[Double], b: DenseVector[Double], x: DenseVector[Double], preconditioner: DenseVector[Double] => DenseVector[Double]): PCGState = {
    require(A.cols == b.length, "matrix and vector dimensions disagree")
    val r = b - A * x
    val z = preconditioner(r)
    val p = z.copy
    PCGState(x = x, r = r, p = p, z = z)
  }

  /** create an iterator of this PCG algorithm */
  def pcgIterator(A: CSCMatrix[Double], b: DenseVector[Double], preconditioner: DenseVector[Double] => DenseVector[Double], initial: PCGState): Iterator[PCGState] = {
    Iterator.iterate(initial)(state => pcgIteration(A, b, preconditioner, state))
  }

  /** a single Fletcher-Reeves Conjugate Gradient iteration (matrix A should not change between iterations) */
  def pcgIteration(A: CSCMatrix[Double], b: DenseVector[Double], preconditioner: DenseVector[Double] => DenseVector[Double], state: PCGState): PCGState = {
    val Ap: DenseVector[Double] = A * state.p
    val rz: Double = state.r.t * state.z
    // alpha: step length
    val alpha: Double = rz / (state.p.t * Ap)
    // update solution
    val x: DenseVector[Double] = state.x + state.p * alpha
    val r: DenseVector[Double] = state.r - Ap * alpha
    val z: DenseVector[Double] = preconditioner(r)
    // conjugate direction update
    val beta: Double = (r.t * z) / rz
    val p: DenseVector[Double] = z + state.p * beta
    PCGState(x = x, r = r, p = p, z = z)
  }

  /** solve A*x = b, sparse A, initial guess is zero */
  def solveSparse(A: CSCMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
    val preconditioner = incompleteCholeskyPreconditioner(A)
    val xInit = DenseVector.zeros[Double](b.length)
    solveSparse(A, b, xInit, preconditioner, 1e-10, 2 * b.length)
  }

  /** use an incomplete Cholesky factorization as preconditioner (factor restricted to sparsity pattern of A, no fill-in) */
  def incompleteCholeskyPreconditioner(A: CSCMatrix[Double]): DenseVector[Double] => DenseVector[Double] = {
    val incL = SparseCholesky.incompleteSparseCholesky(A)
    x => SparseCholesky.substitutionSolver(incL, x)
  }

  /** holds state of a preconditioned conjugate gradient iteration */
  case class PCGState(x: DenseVector[Double], r: DenseVector[Double], p: DenseVector[Double], z: DenseVector[Double])

}
