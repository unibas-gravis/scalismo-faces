/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.faces.landmarks

import scalismo.faces.image.{PixelImage, PixelImageDomain}
import scalismo.faces.image.filter.GeneralMaxConvolution
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{Point1D, _1D}


/**
  * LandmarkDetectionMap is a tagged PixelImage[Double].
  */
trait LandmarkDetectionMap extends ((Int, Int) => Double) {
  def tag: String
  def domain: PixelImageDomain
}

/**
  * The implementation of the LandmarkDetectionMap.
  */
private class LandmarkDetectionMapImplementation(
  override val tag: String,
  private val detectionMap: PixelImage[Double]
) extends LandmarkDetectionMap {
  override def domain: PixelImageDomain = detectionMap.domain
  def apply(x: Int, y: Int): Double = detectionMap(x, y)
}

/**
  * The factory object for a LandmarkDetectionMap.
  */
object LandmarkDetectionMap {

  /**
    * Constructor where everything needs to be specified.
    */
  def apply(tag: String, map: PixelImage[Double], stddevNoiseModel: Double, fp: Double, fn: Double): LandmarkDetectionMap = {
    val correctedMap = correctFalsePositiveAndFalseNegativeRates(map, fp, fn)
    val precalculatedMapIncludingNoiseModel = precalculateIsotropicGaussianNoise(correctedMap, stddevNoiseModel)
    new LandmarkDetectionMapImplementation(tag, precalculatedMapIncludingNoiseModel)
  }

  /**
    * Constructor if the correction for the false-positive and false-negative rates needs to be ommited.
    */
  def fromCorrectedDetection(tag: String, correctedMap: PixelImage[Double], stddevNoiseModel: Double): LandmarkDetectionMap = {
    val precalculatedMapIncludingNoiseModel = precalculateIsotropicGaussianNoise(correctedMap, stddevNoiseModel)
    new LandmarkDetectionMapImplementation(tag, precalculatedMapIncludingNoiseModel)
  }

  /**
    * Construction for a LandmarkDetectionMap without correction nor a precalculated noise model.
    * @note We do not recommend to use this constructor unless you know what you are doing.
    */
  def fromCorrectedPrecalculatedMap(tag: String, precalculatedMapIncludingNoiseModel: PixelImage[Double]): LandmarkDetectionMap = {
    new LandmarkDetectionMapImplementation(tag, precalculatedMapIncludingNoiseModel)
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