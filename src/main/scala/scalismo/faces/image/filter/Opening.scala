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
import scalismo.faces.image.PixelImage

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
