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
        windowFilter(kernelPixels)
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

/**
  * Separable morphological filter (separable structuring element)
  * @param structuringElement 1d structuring element, must be 1 row or column
  * @param windowFilter Filtering function
  */
case class SeparableMorphologicalFilter[A: ClassTag](structuringElement: PixelImage[Boolean], windowFilter: (Seq[A]) => A) extends ImageFilter[A, A] {
  assert(structuringElement.width == 1 || structuringElement.height == 1, "Structuring element must be 1D")

  private val width = structuringElement.width
  private val height = structuringElement.height

  private val rowElement = {
    if (height == 1)
      structuringElement
    else
      structuringElement.transposed
  }

  private val colElement = {
    if (height == 1)
      structuringElement.transposed
    else
      structuringElement
  }

  private val columnFilter = MorphologicalFilter[A](rowElement, windowFilter)
  private val rowFilter = MorphologicalFilter[A](colElement, windowFilter)

  override def filter(image: PixelImage[A]): PixelImage[A] = image.domain match {
    case d: ColumnMajorImageDomain => image.filter(columnFilter).withAccessMode(Repeat()).filter(rowFilter)
    case d: RowMajorImageDomain => image.filter(rowFilter).withAccessMode(Repeat()).filter(columnFilter)
  }
}

object SeparableMorphologicalFilter {
  def lineElement(size: Int): PixelImage[Boolean] = PixelImage.view(size, 1, (x, y) => x >= 0 && x < size)
}

object Dilation {
  /**
    * dilation filter with box element
    * @param size side length of box
    */
  def box(size: Int): SeparableMorphologicalFilter[Double] = {
    SeparableMorphologicalFilter(SeparableMorphologicalFilter.lineElement(size), list => list.max)
  }

  /**
    * general dilation filter with specified structuring element
    * @param structuringElement structuring element, see morphological filter
    */
  def apply(structuringElement: PixelImage[Boolean]): MorphologicalFilter[Double] = {
    MorphologicalFilter[Double](structuringElement, values => values.max)
  }
}

object Erosion {
  /**
    * erosion filter with box element
    * @param size side length of box
    */
  def box(size: Int): SeparableMorphologicalFilter[Double] = {
    SeparableMorphologicalFilter(SeparableMorphologicalFilter.lineElement(size), list => list.min)
  }

  /**
    * general erosion filter with structuring element
    * @param structuringElement structuring element, see morphological filter
    */
  def apply(structuringElement: PixelImage[Boolean]): MorphologicalFilter[Double] = {
    MorphologicalFilter[Double](structuringElement, values => values.min)
  }
}

object Median {
  // extract median value
  private def median(values: Seq[Double]): Double = {
    if(values.length <= 1)
      values.head
    else{
      val sorted = values.sorted
      (sorted(values.length/2) + sorted(values.length - values.length/2)) / 2
    }
  }

  /**
    * median filter with box as its structuring element
    * @param size side length of the box
    */
  def box(size: Int): MorphologicalFilter[Double] = {
    MorphologicalFilter(MorphologicalFilter.boxElement(size), median)
  }

  /**
    * approximate median filter with box as its structuring element, separable (not identical to box)
    * @param size side length of the box
    */
  def separableBox(size: Int): SeparableMorphologicalFilter[Double] = {
    SeparableMorphologicalFilter(SeparableMorphologicalFilter.lineElement(size), median)
  }

  /**
    * general median filter with structuring element
    * @param structuringElement structuring element, see morphological filter
    */
  def apply(structuringElement: PixelImage[Boolean]): MorphologicalFilter[Double] = {
    MorphologicalFilter[Double](structuringElement, median)
  }
}

object Opening {
  def box(size: Int): ImageFilter[Double, Double] = {
    val eroder = Erosion.box(size)
    val dilator = Dilation.box(size)
    image => image.filter(eroder).withAccessMode(Repeat()).filter(dilator)
  }

  def apply(structuringElement: PixelImage[Boolean]): ImageFilter[Double, Double] = {
    val eroder = Erosion(structuringElement)
    val dilator = Dilation(structuringElement)
    image => image.filter(eroder).withAccessMode(Repeat()).filter(dilator)
  }
}

object Closing {
  def box(size: Int): ImageFilter[Double, Double] = {
    val eroder = Erosion.box(size)
    val dilator = Dilation.box(size)
    image => image.filter(dilator).withAccessMode(Repeat()).filter(eroder)
  }

  def apply(structuringElement: PixelImage[Boolean]): ImageFilter[Double, Double] = {
    val eroder = Erosion(structuringElement)
    val dilator = Dilation(structuringElement)
    image => image.filter(dilator).withAccessMode(Repeat()).filter(eroder)
  }
}

object MorphologicalGradient {
  import PixelImage.implicits._

  def box(size: Int): ImageFilter[Double, Double] = {
    val eroder = Erosion.box(size)
    val dilator = Dilation.box(size)
    image => image.filter(dilator) - image.filter(eroder)
  }

  def apply(structuringElement: PixelImage[Boolean]): ImageFilter[Double, Double] = {
    val eroder = Erosion(structuringElement)
    val dilator = Dilation(structuringElement)
    image => image.filter(dilator) - image.filter(eroder)
  }
}
