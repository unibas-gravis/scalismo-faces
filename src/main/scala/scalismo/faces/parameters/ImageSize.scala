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

import scalismo.color.RGBA
import scalismo.faces.image.PixelImageDomain
import scalismo.faces.render.{RenderBuffer, WindowTransform, ZBuffer}

/** parameters of the image size */
case class ImageSize(width: Int, height: Int) {
  val domain = PixelImageDomain(width, height)

  def aspectRatio: Double = width.toDouble / height

  /** create an empty ZBuffer for rendering */
  def zBuffer(bgColor: RGBA): RenderBuffer[RGBA] = ZBuffer[RGBA](width, height, bgColor)

  def screenTransform = WindowTransform(width, height)

  /** scale the image size, keeps aspect ratio */
  def scaleImage(scale: Double): ImageSize = copy(
    width = (width * scale).toInt,
    height = (height * scale).toInt
  )
}
