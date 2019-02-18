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

package scalismo.faces.image

import scalismo.color.ColorSpaceOperations
import scalismo.faces.image.AccessMode.Repeat
import scalismo.faces.numerics.{GenericMultigridPoissonSolver, ImageDomainPoissonSolver}

import scala.reflect.ClassTag

/** Methods to inpaint/blend two images using Poisson Image Editing [roughly Perez 2003 SIGGRAPH (different implementation)] */
class PoissonInpainting[A: ClassTag](solver: ImageDomainPoissonSolver[A])(implicit ops: ColorSpaceOperations[A]) {
  import scalismo.color.ColorSpaceOperations.implicits._

  /** inpainting with seamless cloning method: match gradients of inpainted part with overlay image */
  def seamlessCloning(targetImage: PixelImage[A], overlayImage: PixelImage[Option[A]], left: Int = 0, top: Int = 0): PixelImage[A] = {
    gradientMixedCloning(
      targetImage,
      overlayImage,
      GradientMaps.selectInset,
      left,
      top)
  }

  /** general Poisson image inpainting with arbitrary guidance field, constructed from the two gradient fields */
  def gradientMixedCloning(targetImage: PixelImage[A],
                           overlayImage: PixelImage[Option[A]],
                           gradientMap: ((A, A), (A, A)) => (A, A),
                           left: Int = 0,
                           top: Int = 0): PixelImage[A] = {
    // construct Poisson boundary mask (True on target)
    val localMask = overlayImage.map(p => p.isEmpty)
    val fullMask = PixelImageOperations.padImage(localMask, targetImage.width, targetImage.height, true, left, top)

    // convenience image resizers (lazy)
    def pad(image: PixelImage[A]): PixelImage[A] = PixelImageOperations.padImage(image, targetImage.width, targetImage.height, ops.zero, left, top)
    def crop(image: PixelImage[A]): PixelImage[A] = PixelImageOperations.subImage(image, left, top, overlayImage.width, overlayImage.height)

    // construct gradient images
    val dgdx = PixelImageDifferential.gradX(overlayImage).map(_.getOrElse(ops.zero))
    val dgdy = PixelImageDifferential.gradY(overlayImage).map(_.getOrElse(ops.zero))
    val dg = dgdx.zip(dgdy)

    val dfdx = crop(PixelImageDifferential.gradX(targetImage)) // seems inefficient, but this is all lazy
    val dfdy = crop(PixelImageDifferential.gradY(targetImage))
    val df = dfdx.zip(dfdy)

    // construct guidance field: use stronger gradient inside overlay region
    val gradImage = PixelImage(overlayImage.domain, (x, y) => gradientMap(dg(x, y), df(x, y)))
    val fullGradImage = PixelImageOperations.padImage(gradImage, targetImage.width, targetImage.height, (ops.zero, ops.zero), left, top)

    //    val guidanceField = fullGradImage.mapWithIndex(
    //      (x, y, g) => if (!fullMask(x, y)) Some(g) else None
    //    ).withAccessMode(AccessMode.Mirror())
    //      .buffer

    val guidanceField = PixelImage(fullGradImage.domain, (x, y) => {
      if (!fullMask(x, y))
        Some(fullGradImage(x, y))
      else
        None
    }).withAccessMode(Repeat()).buffer

    blendPoissonGuided(targetImage, guidanceField)
  }

  /** solve the Poisson problem with the given guidance field: Laplace(f) = div(guidance), targetImage is initial guess */
  def blendPoissonGuided(targetImage: PixelImage[A], guidanceField: PixelImage[Option[(A, A)]]): PixelImage[A] = {
    // Poisson image inpainting: use gradient of inset as a "guidance field" ...
    // solve Laplace(f) = div(guidance), f|b = f*|b (Poisson equation) -- using MultigridPoissonSolver
    require(targetImage.domain == guidanceField.domain)

    // extract information from guidanceField
    val guidanceValid = guidanceField.map(_.isDefined)
    val vx = guidanceField.map(p => p.map(t => t._1).getOrElse(ops.zero))
    val vy = guidanceField.map(p => p.map(t => t._2).getOrElse(ops.zero))
    val divImage = PixelImageDifferential.div(vx, vy)

    // mask: where to solve the inset's Poisson equation
    val mask = guidanceValid

    // solve Poisson equation with given solver
    solver.solvePoisson(targetImage, mask, divImage)
  }

  /** inpainting with mixed seamless cloning method: select inpainted to be the stronger gradient of target and overlay */
  def mixedSeamlessCloning(targetImage: PixelImage[A],
                           overlayImage: PixelImage[Option[A]],
                           left: Int = 0,
                           top: Int = 0): PixelImage[A] = {
    gradientMixedCloning(targetImage,
      overlayImage,
      GradientMaps.maxGradientMagnitude,
      left,
      top)
  }

  /** masked inpainting: hard selection of inpainted values within region */
  def inpaintHard(targetImage: PixelImage[A], insetImage: PixelImage[Option[A]], left: Int = 0, top: Int = 0): PixelImage[A] = {
    PixelImage.fromTemplate(targetImage, (x, y) => {
      val x1 = x - left
      val y1 = y - top
      if (insetImage.domain.isDefinedAt(x1, y1))
        insetImage(x1, y1).getOrElse(targetImage(x, y))
      else
        targetImage(x, y)
    })
  }

  /** standard masked soft blending, linear mixing */
  def inpaintSoft(targetImage: PixelImage[A],
                  insetImage: PixelImage[A],
                  mask: PixelImage[Double],
                  left: Int = 0,
                  top: Int = 0): PixelImage[A] = {
    val fullMask = PixelImageOperations.padImage(mask, targetImage.width, targetImage.height, 0.0, left, top)
    val fullInset = PixelImageOperations.padImage(insetImage, targetImage.width, targetImage.height, targetImage(0, 0), left, top)
    PixelImage.fromTemplate(targetImage, (x, y) =>
      fullInset(x, y) * fullMask(x, y) + targetImage(x, y) * (1.0 - fullMask(x, y)))
  }

  private implicit val colorSpaceOpsForOptions = new ColorSpaceOperations[Option[A]] {
    override def add(pix1: Option[A], pix2: Option[A]): Option[A] = for { p <- pix1; q <- pix2 } yield p + q

    override def multiply(pix1: Option[A], pix2: Option[A]): Option[A] = for { p <- pix1; q <- pix2 } yield p multiply q

    override def dot(pix1: Option[A], pix2: Option[A]): Double = (for { p <- pix1; q <- pix2 } yield p dot q).getOrElse(0.0)

    override def scale(pix: Option[A], l: Double): Option[A] = pix.map(p => p * l)

    override val zero: Option[A] = None
    override val dimensionality: Int = ops.dimensionality
  }

  /** module to gather different strategies to calculated the inpainting's target gradient */
  object GradientMaps {
    /** choose the inset gradient everywhere (seamless cloning) [inpainted part should match the inset's gradient] */
    def selectInset(gradInset: (A, A), gradTarget: (A, A)): (A, A) = gradInset

    /** choose the target gradient everywhere (not very useful, reintegrate the image) [inpainted part should match the original's gradient] */
    def selectTarget(gradInset: (A, A), gradTarget: (A, A)): (A, A) = gradTarget

    /** choose the gradient with the larger magnitude (mixed seamless cloning) */
    def maxGradientMagnitude(gradInset: (A, A), gradTarget: (A, A)): (A, A) = {
      val (dfdx, dfdy) = gradTarget
      val (dgdx, dgdy) = gradInset
      val dF = (dfdx x dfdx) + (dfdy x dfdy)
      val dG = (dgdx x dgdx) + (dgdx x dgdy)
      if (dG.normSq > dF.normSq)
        gradInset
      else
        gradTarget
    }

    /** choose inset only if gradient magnitude is above a threshold */
    def gradThreshold(threshold: Double)(gradInset: (A, A), gradTarget: (A, A)): (A, A) = {
      val thSq = threshold * threshold
      val (dfdx, dfdy) = gradTarget
      val (dgdx, dgdy) = gradInset
      val dG = (dgdx x dgdx) + (dgdx x dgdy)
      if (dG.normSq >= threshold * threshold)
        gradInset
      else
        gradTarget
    }
  }
}

object PoissonInpainting {
  /** construct PoissonInpainting with specified Poisson solver */
  def apply[A: ClassTag](solver: ImageDomainPoissonSolver[A])(implicit ops: ColorSpaceOperations[A]) = new PoissonInpainting[A](solver)

  /** construct PoissonInpainting with default solver (currently multigrid) */
  def apply[A: ClassTag](implicit ops: ColorSpaceOperations[A]): PoissonInpainting[A] = {
    val solver = GenericMultigridPoissonSolver[A]
    new PoissonInpainting[A](solver)
  }
}