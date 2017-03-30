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

package scalismo.faces.sampling.face.proposals

import scalismo.faces.color.RGB
import scalismo.faces.parameters.ColorTransform
import scalismo.faces.sampling.evaluators.LogNormalDistribution
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.sampling.{ProposalGenerator, TransitionProbability}
import scalismo.utils.Random

/** Random Gaussian Proposal for Color.gain, Color.contrast and Color.offset */
case class GaussianColorProposal(logSdevGain: RGB, logSdevColorContrast: Double, sdevOffset: RGB)(implicit rnd: Random)
  extends ProposalGenerator[ColorTransform] with TransitionProbability[ColorTransform] {
  override def propose(current: ColorTransform): ColorTransform = {
    val proposedGain = RGB(
      current.gain.r * math.exp(rnd.scalaRandom.nextGaussian() * logSdevGain.r),
      current.gain.g * math.exp(rnd.scalaRandom.nextGaussian() * logSdevGain.g),
      current.gain.b * math.exp(rnd.scalaRandom.nextGaussian() * logSdevGain.b)
    )
    current.copy(gain = proposedGain)

    val proposedContrast = current.colorContrast * math.exp(rnd.scalaRandom.nextGaussian() * logSdevColorContrast)
    current.copy(colorContrast = proposedContrast)

    val proposedOffset = RGB(
      current.offset.r + rnd.scalaRandom.nextGaussian() * sdevOffset.r,
      current.offset.g + rnd.scalaRandom.nextGaussian() * sdevOffset.g,
      current.offset.b + rnd.scalaRandom.nextGaussian() * sdevOffset.b
    )
    current.copy(offset = proposedOffset)
  }

  override def logTransitionProbability(from: ColorTransform, to: ColorTransform): Double = {
    //gain
    LogNormalDistribution.logDensity(to.gain.r/from.gain.r, 0.0, logSdevGain.r) +
      LogNormalDistribution.logDensity(to.gain.g/from.gain.g, 0.0, logSdevGain.g) +
      LogNormalDistribution.logDensity(to.gain.b/from.gain.b, 0.0, logSdevGain.b) +
      // contrast
      LogNormalDistribution.logDensity(to.colorContrast/from.colorContrast, 0.0, logSdevColorContrast) +
      // offset
      GaussianEvaluator.probability(to.offset.r, from.offset.r, sdevOffset.r) +
      GaussianEvaluator.probability(to.offset.g, from.offset.g, sdevOffset.g) +
      GaussianEvaluator.probability(to.offset.b, from.offset.b, sdevOffset.b)
  }

  override def toString: String = {
    s"ColorProposal($logSdevGain,$logSdevColorContrast,$sdevOffset)"
  }
}