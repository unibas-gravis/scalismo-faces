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
import scalismo.geometry._
import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio, TransitionProbability}
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.utils.Random


/**
  * Trait for translation proposals.
  */
trait GaussianTranslationProposal extends ProposalGenerator[Pose] with SymmetricTransitionRatio[Pose] with TransitionProbability[Pose]

/**
  * Gaussian translation proposal for changing a Pose.
  */
object GaussianTranslationProposal {

  /**
    * Constructs a translation proposal, which only shifts in the xy-plane and leaves the z-value constant.
    */
  def apply(sdev: Vector2D) : GaussianTranslationProposal = {
    Gaussian3DTranslationProposalConstantZ(sdev)
  }

  /**
    * Constructs a full 3d translation proposal.
    */
  def apply(sdev: Vector3D) : GaussianTranslationProposal = {
    Gaussian3DTranslationProposal(sdev)
  }

}

/**
  * Random translation proposal in 3D which is parallel to the xy plane.
  * The z-value, i.e. the  distance to the camera is constant.
  */
private[proposals] case class Gaussian3DTranslationProposalConstantZ(sdev: Vector[_2D])(implicit rnd: Random)
    extends GaussianTranslationProposal {
  override def propose(current: Pose): Pose = current.copy(
    translation = Vector(
      current.translation.x + rnd.scalaRandom.nextGaussian() * sdev.x,
      current.translation.y + rnd.scalaRandom.nextGaussian() * sdev.y,
      current.translation.z)
  )

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: Pose, to: Pose): Double = {
    if (to.copy(translation = from.translation) != from)
      Double.NegativeInfinity
    else {
      val diff = to.translation - from.translation
      val px = GaussianEvaluator.logDensity(diff.x, 0, sdev.x)
      val py = GaussianEvaluator.logDensity(diff.y, 0, sdev.y)
      px + py
    }
  }
}

/**
  * Full random translation proposal in 3D.
  */
private[proposals] case class Gaussian3DTranslationProposal(sdev: Vector[_3D])(implicit rnd: Random)
  extends GaussianTranslationProposal {
  override def propose(current: Pose): Pose = current.copy(
    translation = Vector(
      current.translation.x + rnd.scalaRandom.nextGaussian() * sdev.x,
      current.translation.y + rnd.scalaRandom.nextGaussian() * sdev.y,
      current.translation.z + rnd.scalaRandom.nextGaussian() * sdev.z)
  )

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: Pose, to: Pose): Double = {
    if (to.copy(translation = from.translation) != from)
      Double.NegativeInfinity
    else {
      val diff = to.translation - from.translation
      val px = GaussianEvaluator.logDensity(diff.x, 0, sdev.x)
      val py = GaussianEvaluator.logDensity(diff.y, 0, sdev.y)
      val pz = GaussianEvaluator.logDensity(diff.z, 0, sdev.z)
      px + py + pz
    }
  }
}