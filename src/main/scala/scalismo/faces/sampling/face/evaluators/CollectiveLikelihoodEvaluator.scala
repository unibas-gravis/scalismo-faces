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

import scalismo.color.{RGB, RGBA}
import scalismo.faces.image.PixelImage
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.PairEvaluator

/**
 * Collective Likelihood: models the average squared distance of images as normal (valid for many pixels, large
 * averages: Central Limit Theorem) for Details see "Markov Chain Monte Carlo for Automated Face Image Analysis" in IJCV
 * 2016
 *
 * @param sigma
 *   expected standard deviation between image and target pixels (noise assumption, total RMS distance), sigma^2 =
 *   E[d^2]
 * @param relativeVariance
 *   variance of squared differences, in relative units of sigma^4
 */
class CollectiveLikelihoodEvaluator(val sigma: Double, val relativeVariance: Double)
    extends PairEvaluator[PixelImage[RGBA]] {
  override def logValue(reference: PixelImage[RGBA], sample: PixelImage[RGBA]): Double = {
    require(sample.domain == reference.domain, "SqCLTEvaluator: images must be comparable! (different sizes)")

    // Image Loop: sum all fg pixels, CLT enforces noise assumption, no bg model needed
    var sum: Double = 0.0
    var count: Int = 0

    var x: Int = 0
    while (x < reference.width) {
      var y: Int = 0
      while (y < reference.height) {
        val refCol: RGB = reference(x, y).toRGB
        val smp: RGBA = sample(x, y)
        if (smp.a > 1e-4) {
          val diff: RGB = refCol - smp.toRGB
          val normSq: Double = diff.dot(diff)
          sum += normSq
          count += 1
        }
        y += 1
      }
      x += 1
    }
    if (count > 0) { // was something rendered on the image?
      val avg = sum / (sigma * sigma * count)
      val stdNormVariate: Double = (avg - 1) / Math.sqrt(relativeVariance / count)
      -0.5 * Math.log(2 * Math.PI) - 0.5 * (stdNormVariate * stdNormVariate)
    } else Double.NegativeInfinity // nothing was rendered on the image!
  }

  override def toString: String = {
    val builder = new StringBuilder(128)
    builder ++= "SqCLTEvaluator("
    builder ++= sigma.toString
    builder ++= ","
    builder ++= relativeVariance.toString
    builder ++= ")"
    builder.mkString
  }
}

object CollectiveLikelihoodEvaluator {
  def apply(sigma: Double, relativeVariance: Double) = new CollectiveLikelihoodEvaluator(sigma, relativeVariance)
}
