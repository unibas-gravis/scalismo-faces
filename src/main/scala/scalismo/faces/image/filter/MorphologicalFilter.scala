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

package scalismo.faces.image.filter

import scalismo.faces.image.AccessMode._
import scalismo.faces.image._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * Filter the image according to ordinal decisions in a "kernel window" (erode, dilate, median filter) - Morphological operation with block structuring element
  * @param structuringElement Structuring element of morphological operation
  * @param windowFilter Function to extract value from filtering
  */
case class MorphologicalFilter[A: ClassTag](structuringElement: PixelImage[Boolean], windowFilter: (Seq[A]) => A) extends ImageFilter[A, A] {
  override def filter(image: PixelImage[A]): PixelImage[A] = {
    val width = structuringElement.width
    val height = structuringElement.height

    require(image.width >= width && image.height >= height, "Filter window can't be bigger than filtered image")

    /** apply filter at position x, y */
    def perPixel(x: Int, y: Int): A = {
      var kx = 0
      var kernelPixels = new ArrayBuffer[A](width * height)
      while (kx < width) {
        val ix = x + kx - width / 2
        var ky = 0
        while (ky < height) {
          val iy = y + ky - height / 2
          if (structuringElement(kx, ky)) kernelPixels += image(ix, iy)
          ky += 1
        }
        kx += 1
      }
      if (kernelPixels.nonEmpty)
        windowFilter(kernelPixels.toSeq)
      else
        image(x, y)
    }

    if(width <= 0 || height <= 0)
      image
    else
      PixelImage(image.width, image.height, perPixel, Strict())
  }
}

object MorphologicalFilter {
  def boxElement(size: Int): PixelImage[Boolean] = PixelImage.view(size, size, (x, y) => x >= 0 && x < size && y >= 0 && y < size)
}
