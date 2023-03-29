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

package scalismo.faces.sampling.face.evaluators

import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.geometry.{_2D, Point}
import scalismo.sampling.DistributionEvaluator

/** simple box structure to capture landmark region */
case class Box(topLeft: Point[_2D], bottomRight: Point[_2D])

/**
 * evaluate position of landmarks with respect to given box returns 0 if all landmarks are in box, else -infinity
 */
class LandmarksInBoxEvaluator(val box: Box) extends DistributionEvaluator[Seq[TLMSLandmark2D]] {

  private def isLandmarkInBox(lm: TLMSLandmark2D): Boolean = {
    lm.point.x >= box.topLeft.x &&
    lm.point.x <= box.bottomRight.x &&
    lm.point.y >= box.topLeft.y &&
    lm.point.y <= box.bottomRight.y
  }

  override def logValue(landmarks: Seq[TLMSLandmark2D]): Double = {
    if (landmarks.forall(isLandmarkInBox))
      0
    else
      Double.NegativeInfinity
  }
}

object LandmarksInBoxEvaluator {
  def apply(box: Box) = new LandmarksInBoxEvaluator(box)
}
