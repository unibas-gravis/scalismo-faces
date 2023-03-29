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

import scalismo.faces.parameters.MoMoInstance
import scalismo.faces.sampling.evaluators.LogNormalDistribution
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio, TransitionProbability}
import scalismo.utils.Random

/** Gaussian proposal on a sequence of numbers */
case class GaussianParameterProposal(sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[IndexedSeq[Double]]
    with SymmetricTransitionRatio[IndexedSeq[Double]]
    with TransitionProbability[IndexedSeq[Double]] {
  override def propose(current: IndexedSeq[Double]): IndexedSeq[Double] = {
    require(current.nonEmpty, "cannot propose change on empty vector")
    current.map { v => v + sdev * rnd.scalaRandom.nextGaussian() }
  }

  override def logTransitionProbability(from: IndexedSeq[Double], to: IndexedSeq[Double]): Double = {
    require(from.nonEmpty, "cannot calculate transition on empty vector")
    require(from.length == to.length, "IndexedSeqs must be of same length")
    to.iterator.zip(from.iterator).map { case (t, f) => GaussianEvaluator.logDensity(t, f, sdev) }.sum
  }
}

/** LogNormal proposal on a sequence of numbers, scaling */
case class LogNormalScalingParameterProposal(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[IndexedSeq[Double]]
    with TransitionProbability[IndexedSeq[Double]] {

  private def normSq(vec: IndexedSeq[Double]): Double = vec.iterator.map { v => v * v }.sum
  private def dot(a: IndexedSeq[Double], b: IndexedSeq[Double]): Double =
    a.iterator.zip(b.iterator).map { case (u, v) => u * v }.sum

  override def propose(current: IndexedSeq[Double]): IndexedSeq[Double] = {
    require(current.nonEmpty, "cannot propose change on empty vector")
    val factor = math.exp(rnd.scalaRandom.nextGaussian() * logSdev)
    current.map { c => c * factor }
  }

  override def logTransitionProbability(from: IndexedSeq[Double], to: IndexedSeq[Double]): Double = {
    require(from.nonEmpty, "cannot calculate transition on empty vector")
    require(from.length == to.length, "shape coefficients should have the same length")
    val normSqFrom = normSq(from)
    val normSqTo = normSq(to)
    if (
      normSqFrom < 1e-14 || normSqTo < 1e-14 || (math
        .abs(dot(from, to)) / math.sqrt(normSqTo * normSqFrom) - 1.0) < 1e-5
    ) // proposal can only scale vectors
      Double.NegativeInfinity
    else {
      val factor = math.sqrt(normSqTo / normSqFrom)
      LogNormalDistribution.logDensity(factor, 0.0, logSdev)
    }
  }
}

/**
 * proposal to vary the shape parameters of a face with an isotropic Gaussian perturbation independent on each
 * component, same standard deviation
 * @param sdev
 *   standard deviation of proposal
 */
case class GaussianMoMoShapeProposal(sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[MoMoInstance]
    with SymmetricTransitionRatio[MoMoInstance]
    with TransitionProbability[MoMoInstance] {

  private val generator = GaussianParameterProposal(sdev)

  override def propose(current: MoMoInstance): MoMoInstance = current.copy(
    shape = generator.propose(current.shape)
  )

  override def logTransitionProbability(from: MoMoInstance, to: MoMoInstance): Double = {
    require(from.shape.length == to.shape.length, "shape coefficients should have the same length")
    generator.logTransitionProbability(from.shape, to.shape)
  }
}

/**
 * proposal to vary the color parameters of a face with an isotropic Gaussian perturbation
 * @param sdev
 *   standard deviation of the proposal
 */
case class GaussianMoMoColorProposal(sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[MoMoInstance]
    with SymmetricTransitionRatio[MoMoInstance]
    with TransitionProbability[MoMoInstance] {

  private val generator = GaussianParameterProposal(sdev)

  override def propose(current: MoMoInstance): MoMoInstance = current.copy(
    color = generator.propose(current.color)
  )

  override def logTransitionProbability(from: MoMoInstance, to: MoMoInstance): Double = {
    require(from.color.length == to.color.length, "color coefficients should have the same length")
    generator.logTransitionProbability(from.color, to.color)
  }
}

/**
 * proposal to vary the expression parameters of a face with an isotropic Gaussian perturbation independent on each
 * component, same standard deviation
 * @param sdev
 *   standard deviation of proposal
 */
case class GaussianMoMoExpressionProposal(sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[MoMoInstance]
    with SymmetricTransitionRatio[MoMoInstance]
    with TransitionProbability[MoMoInstance] {

  private val generator = GaussianParameterProposal(sdev)

  override def propose(current: MoMoInstance): MoMoInstance = current.copy(
    expression = generator.propose(current.expression)
  )

  override def logTransitionProbability(from: MoMoInstance, to: MoMoInstance): Double = {
    require(from.expression.length == to.expression.length, "expression paramaters must match in length")
    generator.logTransitionProbability(from.expression, to.expression)
  }
}

/**
 * proposal to vary the distance of the shape to the mean (caricature, length of parameter vector)
 * @param logSdev
 *   log of the expected scaling factor
 */
case class GaussianMoMoShapeCaricatureProposal(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[MoMoInstance]
    with TransitionProbability[MoMoInstance] {

  private val generator = LogNormalScalingParameterProposal(logSdev)

  override def propose(current: MoMoInstance): MoMoInstance = current.copy(
    shape = generator.propose(current.shape)
  )

  override def logTransitionProbability(from: MoMoInstance, to: MoMoInstance): Double = {
    require(from.shape.length == to.shape.length, "shape coefficients should have the same length")
    generator.logTransitionProbability(from.shape, to.shape)
  }
}

/**
 * proposal to vary the distance of the color to the mean (caricature, length of parameter vector)
 * @param logSdev
 *   log of the expected scaling factor
 */
case class GaussianMoMoColorCaricatureProposal(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[MoMoInstance]
    with TransitionProbability[MoMoInstance] {

  private def generator = LogNormalScalingParameterProposal(logSdev)

  override def propose(current: MoMoInstance): MoMoInstance = current.copy(
    color = generator.propose(current.color)
  )

  override def logTransitionProbability(from: MoMoInstance, to: MoMoInstance): Double = {
    require(from.color.length == to.color.length, "color coefficients should have the same length")
    generator.logTransitionProbability(from.color, to.color)
  }
}

/**
 * proposal to vary the distance of the expression to the mean (caricature, length of parameter vector)
 * @param logSdev
 *   log of the expected scaling factor
 */
case class GaussianMoMoExpressionCaricatureProposal(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[MoMoInstance]
    with TransitionProbability[MoMoInstance] {

  private def generator = LogNormalScalingParameterProposal(logSdev)

  override def propose(current: MoMoInstance): MoMoInstance = current.copy(
    expression = generator.propose(current.expression)
  )

  override def logTransitionProbability(from: MoMoInstance, to: MoMoInstance): Double = {
    require(from.expression.length == to.expression.length, "color coefficients should have the same length")
    generator.logTransitionProbability(from.expression, to.expression)
  }
}
