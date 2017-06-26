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

import scalismo.faces.image.{PixelImage, PixelImageOperations}

/** Solve Poisson's equation in an image domain with (arbitrary) Dirichlet boundary conditions */
trait ImageDomainPoissonSolver[A] {
  /**
   * solve Poisson's equation
   *
   * @param image initial and boundary values, fixed values of f
   * @param mask  marks domain where solution is required, (true: fill-in trough equation (with f), false: keep fixed (boundary value))
   * @param rhs   right-hand side of Poisson equation
   */
  def solvePoisson(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A]

  /** solve Poisson's equation in bounding box of mask */
  def apply(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = solvePoissonInBoundingBox(image, mask, rhs)

  /** solve Poisson's equation in bounding box of mask */
  def solvePoissonInBoundingBox(image: PixelImage[A], mask: PixelImage[Boolean], rhs: PixelImage[A]): PixelImage[A] = {
    // extract bounding box of mask: do not need to calculate outside the box + boundary
    val bb: (Int, Int, Int, Int) = PixelImageOperations.boundingBox[Boolean](mask, x => x)

    // grow bounding box by 1 pixel in each direction, includes boundary values
    val left = math.max(0, bb._1 - 1)
    val top = math.max(0, bb._2 - 1)
    val right = math.min(image.width - 1, bb._3 + 1)
    val bottom = math.min(image.height - 1, bb._4 + 1)

    // restrict problem to bounding box + 1x1 pixels boundary layer
    val imageBB = PixelImageOperations.subImage(image, left, top, right - left, bottom - top)
    val maskBB = PixelImageOperations.subImage(mask, left, top, right - left, bottom - top)
    val rhsBB = PixelImageOperations.subImage(rhs, left, top, right - left, bottom - top)

    // run solver inside bounding box
    val solutionBB = solvePoisson(imageBB, maskBB, rhsBB)

    // insert solution into full image
    PixelImageOperations.insetView(image, solutionBB, left, top)
  }
}