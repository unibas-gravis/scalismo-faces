/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.faces.image.pyramid

import scalismo.faces.image.PixelImage

/**
 * ImagePyramid trait that offers access to the levels of the pyramid and the number of levels.
 * @tparam A
 *   Pixel type of underlying images in the Pyramid.
 */
trait ImagePyramid[A] {

  /**
   * Number of levels of the pyramid.
   */
  val levels: Int

  /**
   * Sequence of levels. Usually head is the largest image and tail the smallest.
   */
  val level: Seq[PixelImage[A]]
}
