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
class LandmarkMapEvaluator(sdevNoiseModel: Double, maps: LandmarkDetectionMap, renderer: ParametricLandmarksRenderer) extends DistributionEvaluator[RenderParameter] {

  private val precalculatedMap = maps.precalculateIsotropicGaussianNoise(sdevNoiseModel)

  override def logValue(sample: RenderParameter): Double = {
    val lm = renderer.renderLandmark(maps.tag,sample).get
    val point = lm.point
    val x = point.x.toInt
    val y = point.y.toInt
    if (precalculatedMap.domain.isDefinedAt(x, y)) {
      precalculatedMap(x, y)
    } else {
      Double.NegativeInfinity
    }
  }

}

object LandmarkMapEvaluator {

  def apply(sdevNoiseModel: Double, maps: Seq[LandmarkDetectionMap], renderer: ParametricLandmarksRenderer): Unit = {
    val mapEvaluators = maps.map(tm => new LandmarkMapEvaluator(sdevNoiseModel,tm,renderer))
    ProductEvaluator(mapEvaluators:_*)
  }

}