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

import scalismo.faces.parameters.Pose
import scalismo.faces.render.Rotation3D
import scalismo.geometry.{Vector, _3D}
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.sampling.{ProposalGenerator, SymmetricTransition}
import scalismo.utils.Random

/** rotation with a random angle (Gaussian) angle around a given axis */
case class GaussianRotationProposal(axis: Vector[_3D], sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[Pose] with SymmetricTransition[Pose] {

  private val normAxis: Vector[_3D] = axis.normalize

  override def propose(current: Pose): Pose = {
    val angle = rnd.scalaRandom.nextGaussian() * sdev
    val rotation = Rotation3D(angle, normAxis)
    val (pitch, yaw, roll) = Rotation3D.decomposeRotationXYZ(rotation)
    current.copy(
      yaw = current.yaw + yaw,
      pitch = current.pitch + pitch,
      roll = current.roll + roll
    )
  }

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: Pose, to: Pose): Double = {
    // this proposal only changes rotation angles
    if (to.copy(yaw = from.yaw, pitch = from.pitch, roll = from.roll) != from)
      Double.NegativeInfinity
    else {
      val dp = to.pitch - from.pitch
      val dy = to.yaw - from.yaw
      val dr = to.roll - from.roll
      val rot = Rotation3D.fromEulerXYZ(dp, dy, dr)
      if (math.abs(rot.phi) > 1e-5 && (rot.axis dot axis) < 1.0 - 1e-4)
        Double.NegativeInfinity // wrong axis
      else {
        GaussianEvaluator.probability(rot.phi, 0.0, sdev)
      }
    }
  }
}