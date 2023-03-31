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

package scalismo.faces.sampling.face.loggers

import java.io.File

import scalismo.color.RGBA
import scalismo.faces.image.PixelImage
import scalismo.faces.io.PixelImageIO
import scalismo.faces.landmarks.{LandmarksDrawer, TLMSLandmark2D}
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.{ParametricImageRenderer, ParametricLandmarksRenderer}
import scalismo.sampling.loggers.ChainStateLogger

/** render images for logging */
class ImageRenderLogger(renderer: ParametricImageRenderer[RGBA], path: File, fileNamePrefix: String)
    extends ChainStateLogger[RenderParameter] {
  private var counter = 0

  override def logState(sample: RenderParameter): Unit = {
    dumpImage(sample)
    counter += 1
  }

  private def dumpImage(sample: RenderParameter) = {
    val image = renderer.renderImage(sample)
    PixelImageIO.write(image, new File(path, f"$fileNamePrefix$counter%08d.png")).get
  }

  /** draw each sample in front of a background */
  def withBackground(background: PixelImage[RGBA]): ImageRenderLogger = {
    val overlayRenderer = new ParametricImageRenderer[RGBA] {
      override def renderImage(sample: RenderParameter): PixelImage[RGBA] = {
        val image = renderer.renderImage(sample.fitToImageSize(background.width, background.height))
        PixelImage(image.domain, (x, y) => background(x, y).toRGB.blend(image(x, y))).map(_.toRGBA)
      }
    }
    ImageRenderLogger(overlayRenderer, path, fileNamePrefix)
  }

  /** draw landmarks into each image */
  def withLandmarks(landmarks: Set[String],
                    lmRenderer: ParametricLandmarksRenderer,
                    color: RGBA,
                    size: Int
  ): ImageRenderLogger = {

    val lmImageRenderer = new ParametricImageRenderer[RGBA] {
      override def renderImage(sample: RenderParameter): PixelImage[RGBA] = {
        val image = renderer.renderImage(sample)
        val renderedLM: Map[String, Option[TLMSLandmark2D]] =
          (for (lm <- landmarks) yield lm -> lmRenderer.renderLandmark(lm, sample)).toMap
        val lms: IndexedSeq[TLMSLandmark2D] = renderedLM.flatMap(_._2).toIndexedSeq
        LandmarksDrawer.drawLandmarks(image, lms, color, size)
      }
    }
    ImageRenderLogger(lmImageRenderer, path, fileNamePrefix)
  }
}

object ImageRenderLogger {
  def apply(renderer: ParametricImageRenderer[RGBA], path: File, fileNamePrefix: String) =
    new ImageRenderLogger(renderer, path, fileNamePrefix)
}
