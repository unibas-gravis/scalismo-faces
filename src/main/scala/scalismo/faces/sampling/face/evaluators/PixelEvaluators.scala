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

import scalismo.faces.color.{HSV, RGB}
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.PairEvaluator

import scala.math._

/** collection of various pixel color distributions */
object PixelEvaluators {

  /** Gaussian color model, isotropic, 3D (RGB) */
  case class IsotropicGaussianPixelEvaluator(sdev: Double) extends PairEvaluator[RGB] {
    val normalizer: Double = -3 / 2 * log(2 * Pi) - 3 * log(sdev)

    override def logValue(first: RGB, second: RGB): Double = {
      val d2 = pow(first.r - second.r, 2) + pow(first.g - second.g, 2) + pow(first.b - second.b, 2)
      normalizer - 0.5 * d2 / sdev / sdev
    }
  }

  /** Gaussian color model, isotropic, 3D (HSV) */
  case class IsotropicGaussianPixelEvaluatorHSV(sdev: Double) extends PairEvaluator[RGB] {
    val normalizer: Double = -3/2*log(2*Pi) - 3*log(sdev)

    def apply(first: HSV, second: HSV): Double = {
      val d2 = min(min(pow((first.hue - second.hue)/2/Pi,2), pow((first.hue-2*Pi-second.hue)/2/Pi,2)), pow((first.hue+2*Pi-second.hue)/2/Pi,2)) + pow(first.saturation - second.saturation, 2) + pow(first.value - second.value, 2)
      normalizer - 0.5f * d2/sdev/sdev
    }
    override def logValue(f: RGB, s: RGB): Double = {
      val first = HSV(f.toSRGB)
      val second = HSV(s.toSRGB)
      apply(first,second)
    }
  }

  /** constant value of evalution, useful as a background likelihood */
  case class ConstantPixelEvaluator[A](value: Double) extends DistributionEvaluator[A] {
    override def logValue(sample: A): Double = value
  }

  /** truncated Gaussian color distribution to respect value range of [0, 1] per channel */
  case class TruncatedGaussianPixelEvaluator(sdev: Double) extends PairEvaluator[RGB] {
    val variance: Double = sdev * sdev
    val normalizer: Double = 3.0 * (0.5 * math.log(2.0 * math.Pi) + 0.5 * math.log(variance))
    def localNormalizer(channel: Double): Double = {
      math.log(
        breeze.numerics.erf((1.0 - channel) / math.sqrt(2.0) / sdev) -
          breeze.numerics.erf(-channel / math.sqrt(2.0) / sdev)
      )
    }

    override def logValue(first: RGB, second: RGB): Double = {
      val diff = first - second
      val d = diff.dot(diff)
      val finalNormalizer = 3.0 * math.log(0.5) + first.map(c => localNormalizer(c)).sum
      (-d / 2.0 * variance) - normalizer - finalNormalizer
    }
  }

}
