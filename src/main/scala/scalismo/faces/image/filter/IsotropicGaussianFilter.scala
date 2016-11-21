/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.faces.image.filter

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.PixelImage

import scala.reflect.ClassTag

/** isotropic Gaussian blur filter, uses fast separable convolution */
class IsotropicGaussianFilter[A: ClassTag](sigma: Double, windowSize: Int)(implicit ops: ColorSpaceOperations[A]) extends ImageFilter[A, A] {

  val s22 = 2.0 * sigma * sigma
  val N = 1.0 / (math.Pi * s22)
  def gauss = (d2: Double) => N * math.exp(-d2 / s22)

  val m = windowSize / 2
  def d2 = (x: Double) => math.pow(x - m, 2.0)

  val Z = {0 until windowSize map { x => gauss(d2(x)) } sum}

  val kernel = PixelImage[Double](
    windowSize,
    1,
    (x: Int, y: Int) => {gauss(d2(x)) / Z}
  )

  val gaussFilter = SeparableCorrelationFilter[A](kernel, kernel.transposed)

  override def filter(image: PixelImage[A]): PixelImage[A] = gaussFilter.filter(image)
}

object IsotropicGaussianFilter {
  def apply[A: ClassTag](sigma: Double)(implicit ops: ColorSpaceOperations[A]): IsotropicGaussianFilter[A] = {
    val s6 = (sigma * 6).toInt
    val windowSize = s6 + (s6 % 2 - 1)
    new IsotropicGaussianFilter[A](sigma, windowSize)
  }

  def apply[A: ClassTag](sigma: Double, windowSize: Int)(implicit ops: ColorSpaceOperations[A]): IsotropicGaussianFilter[A] = {
    new IsotropicGaussianFilter[A](sigma, windowSize)
  }
}
