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

import scalismo.faces.image.PixelImage

/** image filter: transforms an image */
trait ImageFilter[A, B] {

  /** apply the filter to an image */
  @deprecated("use filter", "0.1")
  def apply(image: PixelImage[A]): PixelImage[B] = filter(image)

  /** apply the filter to an image */
  def filter(image: PixelImage[A]): PixelImage[B]
}

object ImageFilter {
  def apply[A, B](f: PixelImage[A] => PixelImage[B]) = new ImageFilter[A, B] {
    override def filter(image: PixelImage[A]): PixelImage[B] = f(image)
  }
}

