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

import scalismo.faces.image.PixelImage
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.faces.utils.GeneralMaxConvolution
import scalismo.geometry.{Point, Point1D, _1D, _2D}
import scalismo.sampling.DistributionEvaluator

/** evaluate landmark positions in detection maps using a pre-convolved distance model */
case class LandmarksMapEvaluator(sdevNoiseModel: Double, detectionMap: PixelImage[Double]) extends DistributionEvaluator[Point[_2D]] {
  private val probMap = GeneralMaxConvolution.separableMaxConvolution(
    detectionMap,
    IsotropicGaussianPointEvaluator[_1D](sdevNoiseModel).toDistributionEvaluator(Point1D(0f))
  )

  override def logValue(sample: Point[_2D]): Double = {
    val x = sample.x.toInt
    val y = sample.y.toInt
    if (probMap.domain.isDefinedAt(x, y)) {
      probMap(x, y)
    } else {
      Double.NegativeInfinity
    }
  }
}