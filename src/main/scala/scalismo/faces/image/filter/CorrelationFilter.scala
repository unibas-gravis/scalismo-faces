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

import scalismo.color.ColorSpaceOperations
import scalismo.faces.image.PixelImage

import scala.reflect.ClassTag

case class CorrelationFilter[@specialized A: ClassTag](kernel: PixelImage[Double])(implicit ops: ColorSpaceOperations[A]) extends ImageFilter[A, A] {
  require(kernel.width % 2 == 1 && kernel.height % 2 == 1, "discrete convolution only with odd-sized kernel")
  import ColorSpaceOperations.implicits._

  override def filter(image: PixelImage[A]): PixelImage[A] = {
    val kw = kernel.width
    val kh = kernel.height

    def perPixel(x: Int, y: Int): A = {
      var kvsum = ops.zero
      var kx = 0
      while (kx < kw) {
        val ix = x + kx - kw / 2

        var ky = 0
        while (ky < kh) {
          val iy = y + ky - kh / 2
          kvsum += image(ix, iy) * kernel(kx, ky)
          ky += 1
        }
        kx += 1
      }
      kvsum
    }

    PixelImage(image.width, image.height, perPixel(_,_))
  }
}


