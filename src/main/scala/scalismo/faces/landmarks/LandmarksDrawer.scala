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

package scalismo.faces.landmarks

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}

import scalismo.faces.color.RGBA
import scalismo.faces.image.{BufferedImageConverter, PixelImage}

object LandmarksDrawer {
  /** draw landmarks into image */
  def drawLandmarks(image: PixelImage[RGBA], landmarks: Iterable[TLMSLandmark2D], color: RGBA, size: Int): PixelImage[RGBA] = {
    val converter = implicitly[BufferedImageConverter[RGBA]]
    val bufImg: BufferedImage = converter.toBufferedImage(image)
    val g2d = bufImg.createGraphics()
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setPaint(new Color(color.r.toFloat, color.g.toFloat, color.b.toFloat, color.a.toFloat))
    for (lm <- landmarks)
      g2d.fillOval((lm.point.x - size / 2).toInt, (lm.point.y - size / 2).toInt, size, size)
    g2d.dispose()
    converter.toPixelImage(bufImg)
  }
}
