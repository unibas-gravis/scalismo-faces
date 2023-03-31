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

import scalismo.faces.parameters.Camera
import scalismo.faces.sampling.evaluators.LogNormalDistribution
import scalismo.sampling.{ProposalGenerator, TransitionProbability}
import scalismo.utils.Random

/**
 * proposal to change the scaling of the image through the focal length of the camera
 * @param logSdev
 *   log of factor of typical variation, 0.0 is no variation
 */
case class GaussianScalingProposal(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[Camera]
    with TransitionProbability[Camera] {

  override def propose(current: Camera): Camera = {
    val f = math.exp(rnd.scalaRandom.nextGaussian() * logSdev)
    current.copy(focalLength = current.focalLength * f)
  }

  override def logTransitionProbability(from: Camera, to: Camera): Double = {
    if (to.copy(focalLength = from.focalLength) == from) {
      LogNormalDistribution.logDensity(to.focalLength / from.focalLength, 0.0, logSdev)
    } else
      Double.NegativeInfinity
  }

  override def toString: String = s"GaussianScalingProposal($logSdev)"
}
