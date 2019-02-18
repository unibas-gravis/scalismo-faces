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

import scalismo.color.ColorSpaceOperations
import scalismo.faces.image.AccessMode.Repeat
import scalismo.faces.image.{ColumnMajorImageDomain, PixelImage, RowMajorImageDomain}

import scala.reflect.ClassTag

/** filter for a separable correlation/convolution */
case class SeparableCorrelationFilter[@specialized A: ClassTag](kernelRow: PixelImage[Double], kernelCol: PixelImage[Double])(implicit ops: ColorSpaceOperations[A]) extends ImageFilter[A, A] {
  require(kernelCol.width == 1, "column filter is not a column")
  require(kernelRow.height == 1, "row filter is not a row")

  private val columnFilter = CorrelationFilter(kernelCol)
  private val rowFilter = CorrelationFilter(kernelRow)

  override def filter(image: PixelImage[A]): PixelImage[A] = image.domain match {
    case _: ColumnMajorImageDomain => image.filter(columnFilter).withAccessMode(Repeat()).filter(rowFilter)
    case _: RowMajorImageDomain => image.filter(rowFilter).withAccessMode(Repeat()).filter(columnFilter)
  }
}
