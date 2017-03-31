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

import scalismo.faces.landmarks.LandmarkDetectionMap
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.ProductEvaluator

/** evaluate landmark positions in detection maps using a pre-convolved distance model */
case class LandmarkMapEvaluator(stdDevNoiseModel: Double, detectionMap: LandmarkDetectionMap, renderer: ParametricLandmarksRenderer) extends DistributionEvaluator[RenderParameter] {

  override def logValue(sample: RenderParameter): Double = {
    val lm = renderer.renderLandmark(detectionMap.tag,sample).get
    val point = lm.point
    val x = point.x.toInt
    val y = point.y.toInt
    if (detectionMap.domain.isDefinedAt(x, y)) {
      detectionMap(x, y)
    } else {
      Double.NegativeInfinity
    }
  }

}

object LandmarkMapEvaluator {

  def apply(sdevNoiseModel: Double, detectionMaps: Seq[LandmarkDetectionMap], renderer: ParametricLandmarksRenderer): DistributionEvaluator[RenderParameter] = {
    val mapEvaluators = detectionMaps.map(detectionMap => new LandmarkMapEvaluator(sdevNoiseModel,detectionMap,renderer))
    ProductEvaluator(mapEvaluators:_*)
  }

}