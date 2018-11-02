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

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector}
import breeze.optimize.linear.{ConjugateGradient => BreezeCG}
import scalismo.common.ComponentRepresentation
import scalismo.faces.image.{ChannelOperations, PixelImage}

import scala.reflect.ClassTag

/** transform a Poisson equation in the image domain to a linear system (finite differences) */
abstract class LinearSystemPoissonSolver[A](implicit vec: ComponentRepresentation[A]) extends ImageDomainPoissonSolver[A] {
  /** find a good linearization order (currently select column major or row major only) */
  protected def findLinearization(mask: PixelImage[Boolean]): ImageLinearization = {
    // choose between row and column major: shorter side should be major to avoid long distances
    val domain = if (mask.width > mask.height) mask.domain.toColumnMajor else mask.domain.toRowMajor
    val allPoints = domain.points
    // construct reduction index (only inner points), maps full index to reduced index (or -1 if not available)
    var index = -1
    val reducedIndex = allPoints.map {
      case (x, y) =>
        if (mask(x, y)) {
          index += 1
          index
        } else
          -1 // invalid value for outside points
    }
    // keep only pixels which are within the variable domain
    val linearization = allPoints.filter { case (x, y) => mask(x, y) }
    // provides linearization (ordering of (x, y) pairs) and reduced index for each (x, y) pair
    ImageLinearization(linearization, (x, y) => reducedIndex(domain.index(x, y)))
  }

  /** constructs a Laplace matrix, (-Laplace(f) is positive definite) calls addElement for each matrix element */
  protected def buildNegativeLaplaceMatrix(linearization: ImageLinearization, mask: PixelImage[Boolean], addElement: (Int, Int, Double) => Unit): Unit = {
    linearization.order.foreach {
      case (x, y) =>
        // reduced index, (only in inside points)
        val rIndex = linearization.reducedIndex(x, y)
        assert(rIndex != -1, "invalid reduced index, inner point should have one")
        // all neighbours
        val nn = getNeighbours4(x, y)
        // inner neighbours
        val in = nn.filter { case (nx, ny) => mask(nx, ny) }
        // reduced indices of inner neighbours
        val rIndexIN = in.map { case (nx, ny) => linearization.reducedIndex(nx, ny) }
        assert(!rIndexIN.contains(-1), "inner point neighbours with invalid reduced index")
        // set matrix entries for point
        // diagonal value
        addElement(rIndex, rIndex, nn.size)
        // forward neighbouring: symmetric, set both entries now
        rIndexIN.filter(i => i > rIndex).foreach { rIN =>
          addElement(rIndex, rIN, -1)
          addElement(rIN, rIndex, -1)
        }
    }
  }

  /** negative of righthand side: actually solves (-Laplace(f)) = -rhs */
  protected def buildNegativeRHS(linearization: ImageLinearization, image: PixelImage[A], rhs: PixelImage[A], component: Int): DenseVector[Double] = {
    val n = linearization.order.size
    val b = DenseVector.zeros[Double](n)
    linearization.order.foreach {
      case (x, y) =>
        // get index
        val rIndex = linearization.reducedIndex(x, y)
        // value of rhs
        val r = vec.component(rhs(x, y), component)
        // check neighbours: at boundary? add fixed boundary values of neighbours to rhs
        val nn = getNeighbours4(x, y)
        val bn = nn.filter { case (nx, ny) => !linearization.isInside(nx, ny) }
        val boundaryNeighbours = bn.map { case (bx, by) => vec.component(image(bx, by), component) }.sum
        // construct target vector
        b(rIndex) = -r + boundaryNeighbours
    }
    b
  }

  /** find 4-neighbourhood */
  protected def getNeighbours4(x: Int, y: Int): IndexedSeq[(Int, Int)] = {
    IndexedSeq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
  }

  protected def linearizeImage(linearization: ImageLinearization, image: PixelImage[A], component: Int): DenseVector[Double] = {
    val n = linearization.order.size
    val b = DenseVector.zeros[Double](n)
    linearization.order.foreach {
      case (x, y) =>
        // get index
        val rIndex = linearization.reducedIndex(x, y)
        // value of image
        b(rIndex) = vec.component(image(x, y), component)
    }
    b
  }

  protected def reconstructImageComponent(solution: DenseVector[Double], linearization: ImageLinearization, image: PixelImage[A], component: Int): PixelImage[Double] = {
    // reconstruct component image
    PixelImage(image.width, image.height,
      (x, y) => {
        val rIndex = linearization.reducedIndex(x, y)
        if (rIndex != -1)
          solution(rIndex)
        else
          vec.component(image(x, y), component)
      })
  }

  /** provides a linearization (ordering of (x, y) pairs) and reduced index for each (x, y) pair */
  protected case class ImageLinearization(order: IndexedSeq[(Int, Int)], reducedIndex: (Int, Int) => Int) {
    val size = order.size

    def isInside(x: Int, y: Int): Boolean = reducedIndex(x, y) != -1
  }

}

/** solve a Poisson equation in the image domain using a dense linear Cholesky solver (due to the dense matrix only for small systems and a lot of memory) */
class DenseLinearSystemPoissonSolver[A: ClassTag](implicit vec: ComponentRepresentation[A]) extends LinearSystemPoissonSolver[A] {

  /**
   * solve the Poisson equation using a dense linear system solver, careful matrix construction of size (w*h)x(w*h)
   *
   * @param image initial and boundary values, fixed values of f
   * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
   * @param rhs   right-hand side of Poisson equation
   */
  override def solvePoisson(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = {
    // get a good linearization order
    val linearization = findLinearization(mask)
    // system size (only inner points)
    val n = linearization.size

    // build matrix according to linearization order
    val M = DenseMatrix.zeros[Double](n, n)
    buildNegativeLaplaceMatrix(linearization, mask, (i, j, v) => M(i, j) = v)

    // cholesky factorization
    val L: DenseMatrix[Double] = DenseCholesky(M)

    // solve system for each channel of A (reuses the same factorization)
    val imageStack = for (c <- 0 until vec.size) yield {
      // extraction of channel c
      // construct rhs for system, original rhs and fixed boundary values (only component c)
      val b = buildNegativeRHS(linearization, image, rhs, c)
      // solve!
      val solution: DenseVector[Double] = DenseCholesky.substitutionSolver(L, b)
      // reconstruct component image
      reconstructImageComponent(solution, linearization, image, c)
    }
    // compose image stack
    ChannelOperations.composeChannels[A](imageStack)
  }
}

/** solve a Poisson equation using a sparse Cholesky decomposition (efficient for small systems, use PCG for larger systems) */
class SparseCholeskyPoissonSolver[A: ClassTag](permutationStrategy: PermutationStrategy = PermutationStrategy.default)(implicit vec: ComponentRepresentation[A])
    extends LinearSystemPoissonSolver[A] {

  /**
   * solve Poisson's equation using a sparse Cholesky decomposition, solves for each component of A independently
   * performs only a single factorization which can be reused for each component
   *
   * @param image initial and boundary values, fixed values of f
   * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
   * @param rhs   right-hand side of Poisson equation
   */
  override def solvePoisson(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = {
    // get a good linearization order
    val linearization = findLinearization(mask)
    // system size (only inner points)
    val n = linearization.size

    // build matrix according to linearization order
    val matBuilder = new CSCMatrix.Builder[Double](n, n, 5 * n)
    buildNegativeLaplaceMatrix(linearization, mask, (i, j, v) => matBuilder.add(i, j, v))
    val M: CSCMatrix[Double] = matBuilder.result()

    // cholesky factorization
    val L = SparseCholesky.sparseCholesky(M)

    // solve system for each channel of A (reuses the same factorization)
    val imageStack = for (c <- 0 until vec.size) yield {
      // extraction of channel c
      // construct rhs for system, original rhs and fixed boundary values (only component c)
      val b = buildNegativeRHS(linearization, image, rhs, c)
      // solve!
      val solution: DenseVector[Double] = SparseCholesky.substitutionSolver(L, b)

      // reconstruct component image
      reconstructImageComponent(solution, linearization, image, c)
    }

    // compose image stack
    ChannelOperations.composeChannels[A](imageStack)
  }
}

/** conjugate gradient solver for a Poisson system, no preconditioning (only for very small systems, prefer Preconditioned CG) */
class ConjugateGradientPoissonSolver[A: ClassTag](tolerance: Double = 1e-6, maxIterations: Int = Int.MaxValue)(implicit vec: ComponentRepresentation[A]) extends LinearSystemPoissonSolver[A] {
  /**
   * solve Poisson's equation
   *
   * @param image initial and boundary values, fixed values of f
   * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
   * @param rhs   right-hand side of Poisson equation
   */
  override def solvePoisson(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = {
    // get a good linearization order
    val linearization = findLinearization(mask)
    // system size (only inner points)
    val n = linearization.size

    // build matrix according to linearization order
    val matBuilder = new CSCMatrix.Builder[Double](n, n, 5 * n)
    buildNegativeLaplaceMatrix(linearization, mask, (i, j, v) => matBuilder.add(i, j, v))
    val A: CSCMatrix[Double] = matBuilder.result()

    val maxIter = math.min(maxIterations, 5 * A.cols)
    // solve system for each channel of A (reuses the same factorization)
    val imageStack = for (c <- 0 until vec.size) yield {
      // extraction of channel c
      // construct rhs for system, original rhs and fixed boundary values (only component c)
      val b = buildNegativeRHS(linearization, image, rhs, c)
      // construct initial guess from image
      val xInitial = linearizeImage(linearization, image, c)
      // solve!
      val solution: DenseVector[Double] = ConjugateGradient.solveSparse(A, b, xInitial, tolerance, maxIter)
      // reconstruct component image
      reconstructImageComponent(solution, linearization, image, c)
    }
    // compose image stack
    ChannelOperations.composeChannels[A](imageStack)
  }
}

/** conjugate gradient solver for a Poisson system, uses incomplete Cholesky preconditioning (considerably faster than CG) */
class PreconditionedConjugateGradientPoissonSolver[A: ClassTag](tolerance: Double = 1e-6, maxIterations: Int = Int.MaxValue)(implicit vec: ComponentRepresentation[A]) extends LinearSystemPoissonSolver[A] {
  /**
   * solve Poisson's equation
   *
   * @param image initial and boundary values, fixed values of f
   * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
   * @param rhs   right-hand side of Poisson equation
   */
  override def solvePoisson(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = {
    // get a good linearization order
    val linearization = findLinearization(mask)
    // system size (only inner points)
    val n = linearization.size

    // build matrix according to linearization order
    val matBuilder = new CSCMatrix.Builder[Double](n, n, 5 * n)
    buildNegativeLaplaceMatrix(linearization, mask, (i, j, v) => matBuilder.add(i, j, v))
    val A: CSCMatrix[Double] = matBuilder.result()

    // preconditioner
    val precond = PreconditionedConjugateGradient.incompleteCholeskyPreconditioner(A)

    val maxIter = math.min(maxIterations, 5 * A.cols)
    // solve system for each channel of A (reuses the same factorization)
    val imageStack = for (c <- 0 until vec.size) yield {
      // extraction of channel c
      // construct rhs for system, original rhs and fixed boundary values (only component c)
      val b = buildNegativeRHS(linearization, image, rhs, c)
      // construct initial guess from image
      val xInitial = linearizeImage(linearization, image, c)
      // solve!
      val solution: DenseVector[Double] = PreconditionedConjugateGradient.solveSparse(A, b, xInitial, precond, tolerance, maxIter)
      // reconstruct component image
      reconstructImageComponent(solution, linearization, image, c)
    }
    // compose image stack
    ChannelOperations.composeChannels[A](imageStack)
  }
}

/** conjugate gradient solver from breeze (for completeness, do not use, has no preconditioning and is slow, rather use PreconditionedConjugateGradient and maybe ConjugateGradient) */
class BreezeCGPoissonSolver[A: ClassTag](tolerance: Double = 1e-6, maxIterations: Int = Int.MaxValue)(implicit vec: ComponentRepresentation[A]) extends LinearSystemPoissonSolver[A] {
  /**
   * solve Poisson's equation
   *
   * @param image initial and boundary values, fixed values of f
   * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
   * @param rhs   right-hand side of Poisson equation
   */
  override def solvePoisson(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = {
    // get a good linearization order
    val linearization = findLinearization(mask)
    // system size (only inner points)
    val n = linearization.size

    val maxIter = math.min(maxIterations, 2 * n)

    // build matrix according to linearization order
    val matBuilder = new CSCMatrix.Builder[Double](n, n, 5 * n)
    buildNegativeLaplaceMatrix(linearization, mask, (i, j, v) => matBuilder.add(i, j, v))
    val A: CSCMatrix[Double] = matBuilder.result()

    // breeze optimizer
    val cgOpt = new BreezeCG[DenseVector[Double], CSCMatrix[Double]]()

    // solve system for each channel of A (reuses the same factorization)
    val imageStack = for (c <- 0 until vec.size) yield {
      // extraction of channel c
      // construct rhs for system, original rhs and fixed boundary values (only component c)
      val b = buildNegativeRHS(linearization, image, rhs, c)
      // construct initial guess from image
      val xInitial = linearizeImage(linearization, image, c)
      // solve!
      val solution: DenseVector[Double] = cgOpt.minimize(b, A, xInitial)
      // reconstruct component image
      reconstructImageComponent(solution, linearization, image, c)
    }
    // compose image stack
    ChannelOperations.composeChannels[A](imageStack)
  }
}