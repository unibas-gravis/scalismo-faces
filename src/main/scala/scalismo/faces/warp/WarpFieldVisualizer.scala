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

package scalismo.faces.warp

import scalismo.color.RGB
import scalismo.faces.color.HSV
import scalismo.faces.image.PixelImage
import scalismo.geometry.{_2D, EuclideanVector}

object WarpFieldVisualizer {

  /** render warp field as color image with hue indicating the direction of the warp */
  def renderWarpFieldColor(field: PixelImage[EuclideanVector[_2D]]): PixelImage[RGB] = {
    val maxLen = field.values.map(_.norm).max

    def makeRGB(vec: EuclideanVector[_2D]): RGB = {
      val len = vec.norm
      val dir = math.atan2(vec.y, vec.x)
      // map dir to hue
      // map len to value
      val value = len / maxLen
      val saturation = 1.0

      // since HSV to RGB conversion expects a number between 0 and 2 Pi, we need to sanitize dir (atan2 can return negative numbers)
      val hue = if (dir < 0) {
        dir % (2 * Math.PI) + (2 * Math.PI)
      } else {
        dir % (2 * Math.PI)
      }
      HSV(hue, saturation, value).toRGB
    }
    field.map(makeRGB)
  }

}
