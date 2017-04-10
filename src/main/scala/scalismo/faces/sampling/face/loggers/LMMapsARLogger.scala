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

import scalismo.faces.color.RGBA
import scalismo.faces.image.{ImageBuffer, PixelImage}
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.sampling.loggers.{AcceptRejectLogger, ChainStateLogger}
import scalismo.sampling.{DistributionEvaluator, ProposalGenerator}

/** render landmarks positions into image for logging, AcceptReject version */
case class LMMapsARLogger(landmarks: Set[String],
    renderer: ParametricLandmarksRenderer,
    target: PixelImage[RGBA]) extends AcceptRejectLogger[RenderParameter] {
  val outMaps: Map[String, ImageBuffer[Double]] = landmarks.map(l => l -> ImageBuffer.makeInitializedBuffer[Double](target.domain.width, target.domain.height)(0f)).toMap
  private var incs: Long = 0

  override def accept(current: RenderParameter, sample: RenderParameter, generator: ProposalGenerator[RenderParameter], evaluator: DistributionEvaluator[RenderParameter]): Unit = {
    landmarks.foreach { lm =>
      val pos = renderer.renderLandmark(lm, sample).get
      val x = pos.point.x.toInt
      val y = pos.point.y.toInt
      val m = outMaps(lm)
      if (m.domain.isDefinedAt(x, y)) {
        incs = incs + 1
        m.update(x, y, m(x, y) + 1)
      }
    }
  }

  override def reject(current: RenderParameter, sample: RenderParameter, generator: ProposalGenerator[RenderParameter], evaluator: DistributionEvaluator[RenderParameter]): Unit = {
    landmarks.foreach { lm =>
      val pos = renderer.renderLandmark(lm, current).get
      val x = pos.point.x.toInt
      val y = pos.point.y.toInt
      val m = outMaps(lm)
      if (m.domain.isDefinedAt(x, y)) {
        incs = incs + 1
        m.update(x, y, m(x, y) + 1)
      }
    }
  }

}

/** render landmarks positions into image for logging, ChainState version */
case class LMMapsStateLogger(landmarks: Set[String],
    renderer: ParametricLandmarksRenderer,
    target: PixelImage[RGBA]) extends ChainStateLogger[RenderParameter] {
  val outMaps: Map[String, ImageBuffer[Double]] = landmarks.map(l => l -> ImageBuffer.makeInitializedBuffer[Double](target.domain.width, target.domain.height)(0f)).toMap
  private var incs: Long = 0

  override def logState(sample: RenderParameter): Unit = {
    landmarks.foreach { lm =>
      val pos = renderer.renderLandmark(lm, sample).get
      val x = pos.point.x.toInt
      val y = pos.point.y.toInt
      val m = outMaps(lm)
      if (m.domain.isDefinedAt(x, y)) {
        incs = incs + 1
        m.update(x, y, m(x, y) + 1)
      }
    }
  }
}