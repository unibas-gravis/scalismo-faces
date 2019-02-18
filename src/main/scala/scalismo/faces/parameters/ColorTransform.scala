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

package scalismo.faces.parameters

import scalismo.color.RGB
import scalismo.faces.render.ColorTransformWithColorContrast

/** parametrization of color transform in image after rendering (color gain, offset and color contrast) */
case class ColorTransform(gain: RGB, colorContrast: Double, offset: RGB) {
  def transform: ColorTransformWithColorContrast = ColorTransformWithColorContrast(gain, colorContrast, offset)
}

object ColorTransform {
  val neutral = ColorTransform(RGB.White, 1f, RGB.Black)
}
