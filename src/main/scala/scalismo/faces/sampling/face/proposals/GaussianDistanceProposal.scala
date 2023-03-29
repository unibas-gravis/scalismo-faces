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

import scalismo.faces.parameters.RenderParameter
import scalismo.geometry.EuclideanVector
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio, TransitionProbability}
import scalismo.utils.Random

/**
 * Gaussian proposal to vary the distance between the face and the camera with an optional scaling compensation
 * @param sdev
 *   standard deviation of Gaussian proposal, typically in mm
 * @param compensateScaling
 *   if true the focal length of the camera is adjusted to compensate for the apparent size change due to the distance
 *   change (isolated perspective change)
 */
case class GaussianDistanceProposal(sdev: Double, compensateScaling: Boolean)(implicit rnd: Random)
    extends ProposalGenerator[RenderParameter]
    with SymmetricTransitionRatio[RenderParameter]
    with TransitionProbability[RenderParameter] {
  override def propose(current: RenderParameter): RenderParameter = {
    val cameraDistance = current.view.translation.z - current.pose.translation.z

    // limit shift
    val shift = math.min(cameraDistance - 1.0, rnd.scalaRandom.nextGaussian() * sdev)

    val factor = if (compensateScaling) (cameraDistance - shift) / cameraDistance else 1.0
    current.copy(
      pose = current.pose.copy(translation = current.pose.translation + EuclideanVector(0.0, 0.0, shift)),
      camera = current.camera.copy(focalLength = factor * current.camera.focalLength)
    )
  }

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: RenderParameter, to: RenderParameter): Double = {
    if (to.copy(pose = from.pose, camera = from.camera) == from) {
      val diff: Double = to.pose.translation.z - from.pose.translation.z
      GaussianEvaluator.logDensity(diff, 0.0, sdev)
    } else
      Double.NegativeInfinity
  }
}
