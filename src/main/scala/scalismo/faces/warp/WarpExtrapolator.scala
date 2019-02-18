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

package scalismo.faces.warp

import scalismo.faces.image.{PixelImage, PushPullInterpolation}
import scalismo.faces.numerics.{GenericMultigridPoissonSolver, ImageDomainPoissonSolver}
import scalismo.geometry.{EuclideanVector, _2D}

/** completes a warp field to be defined on the whole image */
trait WarpExtrapolator {
  def apply(field: PixelImage[Option[EuclideanVector[_2D]]]): PixelImage[EuclideanVector[_2D]]
}

/** fill missing warp values with no warp (EuclideanVector(0, 0)) */
case object ZeroExtrapolator extends WarpExtrapolator {
  override def apply(field: PixelImage[Option[EuclideanVector[_2D]]]): PixelImage[EuclideanVector[_2D]] = field.map(_.getOrElse(EuclideanVector(0, 0)))
}

/** fill missing warp values with constant value */
case class ConstantWarpExtrapolator(value: EuclideanVector[_2D]) extends WarpExtrapolator {
  override def apply(field: PixelImage[Option[EuclideanVector[_2D]]]): PixelImage[EuclideanVector[_2D]] = field.map(_.getOrElse(value))
}

/** fill missing warp values by Laplace interpolation (solving Laplace equation for undefined regions) */
case class PoissonExtrapolator(solver: ImageDomainPoissonSolver[EuclideanVector[_2D]]) extends WarpExtrapolator {
  override def apply(field: PixelImage[Option[EuclideanVector[_2D]]]): PixelImage[EuclideanVector[_2D]] = {
    val mask = field.map(_.isEmpty).buffer
    val initial = PushPullExtrapolator(4)(field)
    val zeroImage: PixelImage[EuclideanVector[_2D]] = PixelImage(field.width, field.height, (x, y) => EuclideanVector(0f, 0f))
    solver.solvePoisson(initial, mask, zeroImage)
  }
}

object PoissonExtrapolator {
  def apply() = new PoissonExtrapolator(GenericMultigridPoissonSolver[EuclideanVector[_2D]])
}

/** fill missing warp values by Push-Pull interpolation (image pyramid averaging) */
case class PushPullExtrapolator(minSize: Int = 1) extends WarpExtrapolator {
  override def apply(field: PixelImage[Option[EuclideanVector[_2D]]]): PixelImage[EuclideanVector[_2D]] = {
    val mask = field.map(p => if (p.isDefined) 1.0 else 0.0).buffer
    val initial = field.map(_.getOrElse(EuclideanVector(0, 0))).buffer
    PushPullInterpolation.fill(initial, mask, minSize).buffer
  }
}
