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

package scalismo.faces.sampling.evaluators

import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.GaussianEvaluator


/** log normal distribution */
case class LogNormalDistribution(logMean: Double, logSdev: Double) extends DistributionEvaluator[Double] {
  override def logValue(sample: Double): Double = LogNormalDistribution.logDensity(sample, logMean, logSdev)
}

object LogNormalDistribution {
  /** log density value for LogNormal distribution */
  def logDensity(x: Double, logMean: Double, logSdev: Double): Double = {
    if (x > 0.0)
      GaussianEvaluator.logDensity(math.log(x), logMean, logSdev)
    else
      Double.NegativeInfinity
  }
}