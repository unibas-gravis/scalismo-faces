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

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image._
import scalismo.faces.image.filter.{ImageFilter, ResampleFilter}
import scalismo.faces.utils.LanguageUtilities.iterate

import scala.reflect.ClassTag

/** Multigrid parameters */
case class MultigridParameters(vCycles: Int,
                               minSize: Int,
                               prepareIterations: Int,
                               solverIterations: Int,
                               occlusionCorrectionIterations: Int,
                               correctionIterations: Int)

object MultigridParameters {
  /** sane default parameters for image inpainting solutions */
  val default = MultigridParameters(
    vCycles = 5,
    minSize = 8,
    prepareIterations = 10,
    solverIterations = 10,
    occlusionCorrectionIterations = 2,
    correctionIterations = 10)
}

/** Multigrid algorithm to solve Poisson's equation in the image domain for generic pixel type A (efficient but may not fully converge with complicated boundaries) */
class GenericMultigridPoissonSolver[A: ClassTag](parameters: MultigridParameters = MultigridParameters.default)(implicit ops: ColorSpaceOperations[A])
  extends ImageDomainPoissonSolver[A] {

  import ColorSpaceOperations.implicits._

  /** solve the equation: Laplace(f) = rhs, f|fixedMask = boundary, single level relaxations: slow and does not solve "low frequency part"! use solveMultigridPoisson instead */
  def solveSingleLevelPoisson(f: PixelImage[A],
                              mask: PixelImage[Boolean],
                              rhs: PixelImage[A],
                              iterations: Int,
                              hx: Double = 1.0,
                              hy: Double = 1.0,
                              sor: Double = 1.5): PixelImage[A] = {
    iterate(f, iterations)(f => redBlackGaussSeidelRelaxation(f, mask, rhs, hx = hx, hy = hy, sor = sor))
  }

  /**
    * multigrid solver of the Poisson equation: calculates f s.t. Laplace(f) = rhs, f(!mask) = image
    *
    * @param image initial and boundary values, fixed values of f
    * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
    * @param rhs   right-hand side of Poisson equation
    */
  override def solvePoisson(image: PixelImage[A],
                            mask: PixelImage[Boolean],
                            rhs: PixelImage[A]): PixelImage[A] = {
    require(mask.domain == image.domain, "mask must be defined on whole image")
    require(rhs.domain == image.domain, "size of rhs must match size of image and mask")
    // run solver for starting grid with spacing of 1
    iterate(image, parameters.vCycles)(i =>
      recursiveMultigridSolver(
        i,
        mask,
        rhs,
        dx = 1.0,
        dy = 1.0,
        parameters
      ))
  }

  /** calculate the residual image of the Poisson problem: rhs - Laplace(image) */
  def residual(f: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A], hx: Double = 1.0, hy: Double = 1.0): PixelImage[A] = {
    val image = f.withAccessMode(AccessMode.Repeat())
    val dxSq = hx * hx
    val dySq = hy * hy
    PixelImage(f.width, f.height, (x, y) => {
      if (mask(x, y))
        rhs(x, y) - (image(x - 1, y) + image(x + 1, y)) / dxSq - (image(x, y - 1) + image(x, y + 1)) / dySq + (2.0 / dxSq + 2.0 / dySq) *: image(x, y)
      else
        ops.zero
    }, f.accessMode)
  }

  /** perform a single Jacobi relaxation of the Poisson equation: Laplace(f) = rhs */
  def jacobiRelaxation(f: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A], hx: Double, hy: Double, sor: Double = 1.0): PixelImage[A] = {
    val image = f.withAccessMode(AccessMode.Repeat())
    val dxSq = hx * hx
    val dySq = hy * hy
    PixelImage(f.width, f.height, (x, y) => {
      if (mask(x, y))
        image(x, y) * (1.0 - sor) + ((sor * 0.5) / (dxSq + dySq)) *: (dySq *: (image(x - 1, y) + image(x + 1, y)) + dxSq *: (image(x, y - 1) + image(x, y + 1)) - (dxSq * dySq) *: rhs(x, y))
      else
        f(x, y)
    }, image.accessMode)
  }

  /**
    * perform a single red-black Gauss-Seidel relaxation of the Poisson equation: Laplace(f) = rhs
    * Gauss-Seidel relaxation uses updated values directly in the relaxation calculation, red-black performs two passes where the even and odd are updated separately
    *
    * @param sor SOR (successive over-relaxation) constant use 0..1 to slow down, 1..2 to accelerate
    */
  def redBlackGaussSeidelRelaxation(f: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A], hx: Double, hy: Double, sor: Double = 1.0): PixelImage[A] = {
    val dxSq = hx * hx
    val dySq = hy * hy
    val oddFilter = ImageFilter((image: PixelImage[A]) => PixelImage(image.width, image.height, (x, y) => {
      if ((x + y) % 2 == 1 && mask(x, y))
        image(x, y) * (1.0 - sor) + ((sor * 0.5) / (dxSq + dySq)) *: (dySq *: (image(x - 1, y) + image(x + 1, y)) + dxSq *: (image(x, y - 1) + image(x, y + 1)) - (dxSq * dySq) *: rhs(x, y))
      else
        image(x, y)
    }, f.accessMode))

    val evenFilter = ImageFilter((image: PixelImage[A]) => PixelImage(image.width, image.height, (x, y) => {
      if ((x + y) % 2 == 0 && mask(x, y))
        image(x, y) * (1.0 - sor) + ((sor * 0.5) / (dxSq + dySq)) *: (dySq *: (image(x - 1, y) + image(x + 1, y)) + dxSq *: (image(x, y - 1) + image(x, y + 1)) - (dxSq * dySq) *: rhs(x, y))
      else
        image(x, y)
    }, f.accessMode))

    f.withAccessMode(AccessMode.Repeat()).filter(oddFilter).filter(evenFilter)
  }

  // recursive MG implementation
  private def recursiveMultigridSolver(f: PixelImage[A],
                                       mask: PixelImage[Boolean],
                                       rhs: PixelImage[A],
                                       dx: Double,
                                       dy: Double,
                                       parameters: MultigridParameters): PixelImage[A] = {

    val width = f.width
    val height = f.height

    val halfWidth = restrictSize(width)
    val halfHeight = restrictSize(height)

    //println(f"level: size=$width%3dx$height%3d: dx=$dx, dy=$dy")

    // check size of mask on next coarser level: do we need to visit or solve here?
    val restrictedMask = restrictMask(mask, halfWidth, halfHeight)
    val restrictedMaskArea = restrictedMask.map(if (_) 1.0 else 0.0).values.sum

    // check maximal recursion depth, minimal image size must be respected
    if (width > parameters.minSize && height > parameters.minSize && restrictedMaskArea > 0.25 * parameters.minSize * parameters.minSize) {
      // a few relaxation steps to get an approximate solution
      val approximate = solveSingleLevelPoisson(f, mask, rhs, parameters.prepareIterations, hx = dx, hy = dy)
      // restrict everything to a coarser level
      // solve on coarser level
      val residualImage = residual(approximate, mask, rhs, dx, dy)

      val restrictedResidual = restrict(residualImage, halfWidth, halfHeight)
      val restrictedResidualMasked = PixelImage.fromTemplate(restrictedResidual, (x, y) => if (restrictedMask(x, y)) restrictedResidual(x, y) else ops.zero)

      val lowerLevelCorrection = recursiveMultigridSolver(
        PixelImage(halfWidth, halfHeight, (x, y) => ops.zero, AccessMode.Repeat()),
        restrictedMask,
        restrictedResidualMasked,
        dx * width.toDouble / halfWidth,
        dy * height.toDouble / halfHeight,
        parameters
      )
      val correction = prolongate(lowerLevelCorrection, width, height)
      // apply residual correction
      val withProlongationCorrection = PixelImage(width, height, (x, y) => {
        if (mask(x, y)) // enforce boundary values
          approximate(x, y) + correction(x, y)
        else
          f(x, y)
      }).withAccessMode(f.accessMode)
      // fix boundary values which are not corrected in the lower level
      val maskC = prolongate(restrictedMask.map(b => if (b) 1.0 else 0.0), width, height).map(_ > 0.5)
      val occludedInLower = PixelImage.fromTemplate(mask, (x, y) => mask(x, y) && !maskC(x, y))
      // relax pixels which are occluded in lower level: adapt only these pixels
      val occlusionCorrected = solveSingleLevelPoisson(withProlongationCorrection, occludedInLower, rhs, parameters.occlusionCorrectionIterations, hx = dx, hy = dy)

      // fix some remaining high-frequency errors of the restriction/prolongation process
      solveSingleLevelPoisson(occlusionCorrected, mask, rhs, parameters.correctionIterations, hx = dx, hy = dy)
    } else {
      solveSingleLevelPoisson(f, mask, rhs, parameters.solverIterations, hx = dx, hy = dy)
    }

  }

  private def restrictSize(n: Int): Int = math.round(n / 1.25).toInt

  /** pyramid / multigrid downsampling step: restrict */
  private def restrict[B: ClassTag](image: PixelImage[B], w: Int, h: Int)(implicit ops: ColorSpaceOperations[B]): PixelImage[B] = {
    ResampleFilter.resampleImage(image.withAccessMode(AccessMode.Repeat()), w, h, InterpolationKernel.BilinearKernel)
  }

  /** pyramid / multigrid downsampling step: boundary mask, treat specially: convert to implicit representation which can be interpolated and then convert back */
  private def restrictMask(mask: PixelImage[Boolean], w: Int, h: Int): PixelImage[Boolean] = {
    restrict(mask.map(b => if (b) 1.0 else 0.0), w, h).map(m => m > 0.5)
  }

  /** pyramid / multigrid upsampling: prolongate */
  private def prolongate[B: ClassTag](image: PixelImage[B], w: Int, h: Int)(implicit ops: ColorSpaceOperations[B]): PixelImage[B] = {
    ResampleFilter.resampleImage(image.withAccessMode(AccessMode.Repeat()), w, h, InterpolationKernel.BilinearKernel)
  }
}

object GenericMultigridPoissonSolver {
  // we need to generate an instance due to pixel type parameter A
  def apply[A: ClassTag](implicit ops: ColorSpaceOperations[A]) = new GenericMultigridPoissonSolver[A]()
}
