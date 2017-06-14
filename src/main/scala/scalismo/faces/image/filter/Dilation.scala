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
