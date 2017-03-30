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

import scalismo.faces.image.PixelImage
import scalismo.faces.image.filter.GeneralMaxConvolution
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{Point1D, _1D}

class LandmarkDetectionMap(val tag: String, val map: PixelImage[Double]) {

  def precalculateIsotropicGaussianNoise(stddevNoiseModel: Double) = {
    GeneralMaxConvolution.separable2D(
      map,
      IsotropicGaussianPointEvaluator[_1D](stddevNoiseModel).toDistributionEvaluator(Point1D(0f))
    )
  }

}

object LandmarkDetectionMap {

  def apply(tag: String, map: PixelImage[Double], fp: Double, fn: Double): LandmarkDetectionMap = {
    val correctedMap = correctFpFnRates(map,fp,fn)
    new LandmarkDetectionMap(tag,correctedMap)
  }

  def fromCorrectedDetection(tag: String, map: PixelImage[Double]): LandmarkDetectionMap = {
    new LandmarkDetectionMap(tag,map)
  }

  private def correctFpFnRates(in: PixelImage[Double], falsePositiveRate: Double, falseNegativeRate: Double): PixelImage[Double] = {
    in.map(e => e * (1.0 - (falsePositiveRate + falseNegativeRate)) + falseNegativeRate)
  }

}