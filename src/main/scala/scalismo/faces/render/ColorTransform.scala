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

package scalismo.faces.render

import breeze.linalg.{inv, DenseMatrix, DenseVector}
import scalismo.color.RGB
import scalismo.color.ColorSpaceOperations.implicits._
import scalismo.geometry.{_3D, SquareMatrix}

/** color transform: map color values */
trait ColorTransform extends (RGB => RGB)

/** General color transform, affine with matrix and shift */
case class AffineColorTransform(A: SquareMatrix[_3D], b: RGB) extends ColorTransform {
  def apply(c: RGB): RGB = RGB(A * c.toVector) + b
  def invert: AffineColorTransform = {
    val Ainv = SquareMatrix.inv(A)
    AffineColorTransform(Ainv, RGB(Ainv * (-b.toVector)))
  }
}

/** Color transform to adapt white point and black point (gain and offset) */
case class WhiteAndBlackPointColorTransform(white: RGB, black: RGB) extends ColorTransform {
  override def apply(color: RGB): RGB = white x color + black
  def invert: WhiteAndBlackPointColorTransform = {
    val Winv = RGB(1f / white.r, 1f / white.g, 1f / white.b)
    WhiteAndBlackPointColorTransform(Winv, Winv x black * (-1f))
  }
}

/**
 * Color transform which adapts the white point (through scaling, "gain"), the color contrast (mixing with gray) and the
 * black point (offset)
 */
case class ColorTransformWithColorContrast(gain: RGB, colorContrast: Double, offset: RGB) extends ColorTransform {
  self =>
  def apply(color: RGB): RGB = {
    val colorMixed = color * colorContrast + RGB(color.luminance * (1 - colorContrast))
    RGB(
      gain.r * colorMixed.r + offset.r,
      gain.g * colorMixed.g + offset.g,
      gain.b * colorMixed.b + offset.b
    )
  }

  /** get the inverse transform, note: only exists if colorConstrast != 0 */
  def invert: ColorTransform = new ColorTransform {
    private val c = colorContrast
    private val A = DenseMatrix((c * (1 - 0.3) + 0.3, 0.59 - 0.59 * c, 0.11 - 0.11 * c),
                                (0.3 - 0.3 * c, c * (1 - 0.59) + 0.59, 0.11 - 0.11 * c),
                                (0.3 - 0.3 * c, 0.59 - 0.59 * c, c * (1 - 0.11) + 0.11)
    )

    private val Ainv = inv(A)

    override def apply(color: RGB): RGB = {
      val mixed = (color - offset) / gain
      val b: DenseVector[Double] = Ainv * DenseVector[Double](mixed.r, mixed.g, mixed.b)
      RGB(b(0), b(1), b(2))
    }

    def invert: ColorTransformWithColorContrast = self
  }
}
