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

import scalismo.faces.FacesTestSuite
import scalismo.faces.color.RGB
import scalismo.faces.common.ComponentRepresentation
import scalismo.faces.image.{AccessMode, PixelImage, PixelImageDifferential}

/** tests for Poisson solvers in the image domain (used for inpainting) */
class ImageDomainPoissonSolverTests extends FacesTestSuite {
  import PixelImage.implicits._

  // setup problem (small enough to solve within reasonable time and memory)
  val w = 151
  val h = 149

  // transfer a zero-gradient into a color gradient, expected behavior: reconstruct color gradient
  val target: PixelImage[Double] = PixelImage(w, h, (x, y) => y.toDouble / h)
  val donor: PixelImage[Double] = PixelImage(w, h, (x, y) => 0.0)

  val boxX = 33
  val boxY = 74
  val mask = PixelImage(target.domain, (x, y) =>
    x >= boxX && x < target.width - boxX && y >= boxY && y < target.height - boxY
  ).withAccessMode(AccessMode.Repeat())

  // initial values: constant black, boundary values are fixed to target
  val initialWithBoundary = PixelImage(target.domain, (x, y) => if (mask(x, y)) 0.0 else target(x, y)).withAccessMode(AccessMode.Repeat())

  // right-hand-side: laplace of donor (zero actually)
  val lapDonor = PixelImageDifferential.laplace4NN(donor)

  // optimal solution
  val optimalSolution = target

  // target values
  val maxResidual = 1.0e-10
  val maxReconstructionError = 1.0e-10 * w * h

  // double vectorizer
  implicit val doubleVectorizer = new ComponentRepresentation[Double] {
    override def fromArray(arr: Array[Double]): Double = arr(0)
    override def component(color: Double, index: Int): Double = color.toDouble
    override def fromComponents(comp: (Int) => Double): Double = comp(0)
    override val size: Int = 1
  }

  // multigrid

  // multigrid settings
  val param = MultigridParameters.default.copy(vCycles = 4)

  describe("The Multigrid Poisson solver for [A], with A=Double") {
    val solver = new GenericMultigridPoissonSolver[Double](param)
    val solution = solver.solvePoisson(initialWithBoundary, mask, lapDonor)

    it("should work with expected precision, residual of Poisson equation") {
      val residual = solver.residual(solution, mask, lapDonor).map(x => x * x).values.sum
      residual should be < maxResidual
    }

    it("should work with expected precision, reconstruction error") {
      val sqDiff: Double = (target - solution).normSq
      sqDiff should be < maxReconstructionError
    }
  }

  describe("The Multigrid Poisson solver for [A], with A=RGB") {
    def toRGB(image: PixelImage[Double]) = image.map(v => RGB(v))
    val initRGB = toRGB(initialWithBoundary)
    val lapDonorRGB = toRGB(lapDonor)

    val solver = new GenericMultigridPoissonSolver[RGB](param)
    val solution = solver.solvePoissonInBoundingBox(initRGB, mask, lapDonorRGB)

    it("should work with expected precision, residual of Poisson equation") {
      val residual = solver.residual(solution, mask, lapDonorRGB).map(c => c.toVector.norm2).values.sum / 3
      residual should be < maxResidual
    }

    it("should work with expected precision, reconstruction error") {
      val sqDiff = (target.map(f => RGB(f)) - solution).normSq / 3
      sqDiff should be < maxReconstructionError
    }
  }

  // Sparse Cholesky
  describe("The sparse Cholesky Poisson solver for [A], with A=Double") {
    val solver = new SparseCholeskyPoissonSolver[Double]
    val solution = solver.solvePoissonInBoundingBox(initialWithBoundary, mask, lapDonor)

    it("should work with expected precision, reconstruction error") {
      val sqDiff: Double = (target - solution).normSq
      sqDiff should be < maxReconstructionError
    }
  }

  describe("The sparse Cholesky Poisson solver for [A], with A=RGB") {
    def toRGB(image: PixelImage[Double]) = image.map(v => RGB(v))
    val initRGB = toRGB(initialWithBoundary)
    val lapDonorRGB = toRGB(lapDonor)

    val solver = new SparseCholeskyPoissonSolver[RGB]
    val solution = solver.solvePoissonInBoundingBox(initRGB, mask, lapDonorRGB)

    it("should work with expected precision, reconstruction error") {
      val sqDiff = (target.map(f => RGB(f)) - solution).normSq / 3
      sqDiff should be < maxReconstructionError
    }
  }

  // Conjugate Gradient
  describe("The conjugate gradient Poisson solver for [A], with A=Double") {
    val solver = new ConjugateGradientPoissonSolver[Double]
    val solution = solver.solvePoissonInBoundingBox(initialWithBoundary, mask, lapDonor)

    it("should work with expected precision, reconstruction error") {
      val sqDiff: Double = (target - solution).normSq
      sqDiff should be < maxReconstructionError
    }
  }

  describe("The conjugate gradient Poisson solver for [A], with A=RGB") {
    def toRGB(image: PixelImage[Double]) = image.map(v => RGB(v))
    val initRGB = toRGB(initialWithBoundary)
    val lapDonorRGB = toRGB(lapDonor)

    val solver = new ConjugateGradientPoissonSolver[RGB]
    val solution = solver.solvePoissonInBoundingBox(initRGB, mask, lapDonorRGB)

    it("should work with expected precision, reconstruction error") {
      val sqDiff = (target.map(f => RGB(f)) - solution).normSq / 3
      sqDiff should be < maxReconstructionError
    }
  }

  // Preconditioned Conjugate Gradient
  describe("The preconditioned conjugate gradient Poisson solver for [A], with A=Double") {
    val solver = new PreconditionedConjugateGradientPoissonSolver[Double]
    val solution = solver.solvePoissonInBoundingBox(initialWithBoundary, mask, lapDonor)

    it("should work with expected precision, reconstruction error") {
      val sqDiff: Double = (target - solution).normSq
      sqDiff should be < maxReconstructionError
    }
  }

  describe("The preconditioned conjugate gradient Poisson solver for [A], with A=RGB") {
    def toRGB(image: PixelImage[Double]) = image.map(v => RGB(v))
    val initRGB = toRGB(initialWithBoundary)
    val lapDonorRGB = toRGB(lapDonor)

    val solver = new PreconditionedConjugateGradientPoissonSolver[RGB]
    val solution = solver.solvePoissonInBoundingBox(initRGB, mask, lapDonorRGB)

    it("should work with expected precision, reconstruction error") {
      val sqDiff = (target.map(f => RGB(f)) - solution).normSq / 3
      sqDiff should be < maxReconstructionError
    }
  }

  // dense Cholesky
  describe("The dense Cholesky Poisson solver for [A], with A=Double") {
    val solver = new DenseLinearSystemPoissonSolver[Double]
    val solution = solver.solvePoissonInBoundingBox(initialWithBoundary, mask, lapDonor)

    it("should work with expected precision, reconstruction error") {
      val sqDiff: Double = (target - solution).normSq
      sqDiff should be < maxReconstructionError
    }
  }

  describe("The dense Cholesky Poisson solver for [A], with A=RGB") {
    def toRGB(image: PixelImage[Double]) = image.map(v => RGB(v))
    val initRGB = toRGB(initialWithBoundary)
    val lapDonorRGB = toRGB(lapDonor)

    val solver = new DenseLinearSystemPoissonSolver[RGB]
    val solution = solver.solvePoissonInBoundingBox(initRGB, mask, lapDonorRGB)

    it("should work with expected precision, reconstruction error") {
      val sqDiff = (target.map(f => RGB(f)) - solution).normSq / 3
      sqDiff should be < maxReconstructionError
    }
  }

  // Breeze Conjugate Gradient
  describe("Breeze's conjugate gradient Poisson solver for [A], with A=Double") {
    val solver = new BreezeCGPoissonSolver[Double]
    val solution = solver.solvePoissonInBoundingBox(initialWithBoundary, mask, lapDonor)

    it("should work with expected precision, reconstruction error") {
      val sqDiff: Double = (target - solution).normSq
      sqDiff should be < maxReconstructionError
    }
  }

  describe("Breeze's conjugate gradient Poisson solver for [A], with A=RGB") {
    def toRGB(image: PixelImage[Double]) = image.map(v => RGB(v))
    val initRGB = toRGB(initialWithBoundary)
    val lapDonorRGB = toRGB(lapDonor)

    val solver = new BreezeCGPoissonSolver[RGB]
    val solution = solver.solvePoissonInBoundingBox(initRGB, mask, lapDonorRGB)

    it("should work with expected precision, reconstruction error") {
      val sqDiff = (target.map(f => RGB(f)) - solution).normSq / 3
      sqDiff should be < maxReconstructionError
    }
  }
}
