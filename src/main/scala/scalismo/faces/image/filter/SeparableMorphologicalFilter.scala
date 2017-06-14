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

import scalismo.faces.image.AccessMode.Repeat
import scalismo.faces.image.{ColumnMajorImageDomain, PixelImage, RowMajorImageDomain}

import scala.reflect.ClassTag

/**
  * Separable morphological filter (separable structuring element)
  * @param rowElement 1d structuring element, must be 1 row or column
  * @param windowFilter Filtering function
  */
case class SeparableMorphologicalFilter[A: ClassTag](rowElement: PixelImage[Boolean], colElement: PixelImage[Boolean], windowFilter: (Seq[A]) => A) extends ImageFilter[A, A] {
  require(rowElement.height == 1 && colElement.width == 1, "Structuring elements must be 1D")

  private val width = rowElement.width
  private val height = rowElement.height

  private val columnFilter = MorphologicalFilter[A](rowElement, windowFilter)
  private val rowFilter = MorphologicalFilter[A](colElement, windowFilter)

  override def filter(image: PixelImage[A]): PixelImage[A] = image.domain match {
    case d: ColumnMajorImageDomain => image.filter(columnFilter).withAccessMode(Repeat()).filter(rowFilter)
    case d: RowMajorImageDomain => image.filter(rowFilter).withAccessMode(Repeat()).filter(columnFilter)
  }
}

object SeparableMorphologicalFilter {
  /** create a filter with a structuring element el1d * el1d.t */
  def apply[A: ClassTag](structElement1D: PixelImage[Boolean], windowFilter: (Seq[A]) => A): SeparableMorphologicalFilter[A] = {
    require(structElement1D.width == 1 || structElement1D.height == 1, "structuring element needs to be 1D")
    val rowElement = if (structElement1D.height == 1) structElement1D else structElement1D.transposed
    SeparableMorphologicalFilter(rowElement, rowElement.transposed, windowFilter)
  }

  /** 1d line structuring element, row */
  def lineElement(size: Int): PixelImage[Boolean] = PixelImage.view(size, 1, (x, y) => x >= 0 && x < size)
}
