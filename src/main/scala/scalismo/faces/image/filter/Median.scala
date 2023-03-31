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

import scalismo.faces.image.PixelImage

/** median image filter */
object Median {
  // extract median value
  private def median(values: Seq[Double]): Double = {
    require(values.nonEmpty, "median needs non-empty (pixel value) sequence")
    if (values.length == 1)
      values.head
    else {
      val sorted = values.sorted
      (sorted(values.length / 2) + sorted(values.length - values.length / 2)) / 2
    }
  }

  /**
   * median filter with box as its structuring element
   * @param size
   *   side length of the box
   */
  def box(size: Int): MorphologicalFilter[Double] = {
    MorphologicalFilter(MorphologicalFilter.boxElement(size), median)
  }

  /**
   * approximate median filter with box as its structuring element, separable (not identical to box)
   * @param size
   *   side length of the box
   */
  def separableBox(size: Int): SeparableMorphologicalFilter[Double] = {
    SeparableMorphologicalFilter(SeparableMorphologicalFilter.lineElement(size), median)
  }

  /**
   * general median filter with structuring element
   * @param structuringElement
   *   structuring element, see morphological filter
   */
  def apply(structuringElement: PixelImage[Boolean]): MorphologicalFilter[Double] = {
    MorphologicalFilter[Double](structuringElement, median)
  }
}
