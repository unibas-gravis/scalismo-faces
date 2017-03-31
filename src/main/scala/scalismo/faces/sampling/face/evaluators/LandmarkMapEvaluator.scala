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
import scalismo.faces.image.filter.GeneralMaxConvolution
import scalismo.faces.landmarks.LandmarkDetectionMap
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{Point1D, _1D}
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.ProductEvaluator

/**
 * The LandmarkMapEvaluator evaluates landmark positions by a simple look-up in a LandmarkDetectionMap. These
 * maps usually include a noise-model through precomputation.
 */
case class LandmarkMapEvaluator(
  detectionMap: LandmarkDetectionMap,
  renderer: ParametricLandmarksRenderer
) extends DistributionEvaluator[RenderParameter] {

  override def logValue(sample: RenderParameter): Double = {
    val landmarkPosition = renderer.renderLandmark(detectionMap.tag, sample).get
    val point = landmarkPosition.point
    val x = point.x.toInt
    val y = point.y.toInt
    if (detectionMap.values.domain.isDefinedAt(x, y)) {
      detectionMap.values(x, y)
    } else {
      Double.NegativeInfinity
    }
  }

}

object LandmarkMapEvaluator {

  /**
   * Convenience constructor for combining multiple landmark map evaluators in a ProductEvaluator.
   */
  def apply(
    detectionMaps: Seq[LandmarkDetectionMap],
    renderer: ParametricLandmarksRenderer,
    stddevNoiseModel: Double,
    falsePositiveRate: Double,
    falseNegativeRate: Double
  ): ProductEvaluator[RenderParameter] = {
    val mapEvaluators = detectionMaps.map { detectionMap =>
      val correctedDetections = correctFalsePositiveAndFalseNegativeRates(detectionMap.values, falseNegativeRate, falseNegativeRate)
      val correctedDetectionsIncludingNoise = precalculateIsotropicGaussianNoise(correctedDetections, stddevNoiseModel)
      new LandmarkMapEvaluator(LandmarkDetectionMap(detectionMap.tag, correctedDetectionsIncludingNoise), renderer)
    }
    ProductEvaluator(mapEvaluators: _*)
  }

  /**
   * Correctes for the false-positive and false-negative rates of the detector in the detection map.
   */
  private def correctFalsePositiveAndFalseNegativeRates(in: PixelImage[Double], falsePositiveRate: Double, falseNegativeRate: Double): PixelImage[Double] = {
    in.map(e => e * (1.0 - (falsePositiveRate + falseNegativeRate)) + falseNegativeRate)
  }

  /**
   * Precalculates a new map with included isotropic Gaussian noise model.
   */
  private def precalculateIsotropicGaussianNoise(detectionMap: PixelImage[Double], stddevNoiseModel: Double) = {
    GeneralMaxConvolution.separable2D(
      detectionMap,
      IsotropicGaussianPointEvaluator[_1D](stddevNoiseModel).toDistributionEvaluator(Point1D(0f))
    )
  }

}