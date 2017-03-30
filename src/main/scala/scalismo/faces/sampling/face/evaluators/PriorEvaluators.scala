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

import scalismo.faces.parameters.RenderParameter
import scalismo.sampling.DistributionEvaluator

/**
 * collection of prior distributions
 */
object PriorEvaluators {

  /** Gaussian shape prior of statistical/probabilistic model */
  case class GaussianShapePrior(mean: Double, sdev: Double) extends DistributionEvaluator[RenderParameter] {

    override def logValue(rps: RenderParameter): Double = {
      val shapeCoeff = rps.momo.shape
      if (shapeCoeff.isEmpty) {
        0.0
      } else {
        val dDiff = shapeCoeff.foldLeft(0.0) { (z: Double, v: Double) => z + math.pow(v - mean, 2) / sdev / sdev }
        val dNorm = -0.5 * shapeCoeff.size * math.log(2 * math.Pi) - shapeCoeff.size * math.log(sdev)
        dNorm - 0.5 * dDiff
      }
    }

  }

  /** Gaussian texture prior of statistical/probabilistic model */
  case class GaussianTexturePrior(mean: Double, sdev: Double) extends DistributionEvaluator[RenderParameter] {

    override def logValue(rps: RenderParameter): Double = {
      val colorCoeff = rps.momo.color
      if (colorCoeff.isEmpty) {
        0.0
      } else {
        val dDiff = colorCoeff.foldLeft(0.0) { (z: Double, v: Double) => z + math.pow(v - mean, 2) / sdev / sdev }
        val dNorm = -0.5 * colorCoeff.size * math.log(2 * math.Pi) - colorCoeff.size * math.log(sdev)
        dNorm - 0.5 * dDiff
      }
    }

  }

  /** Gaussian expression prior of statistical/probabilistic model */
  case class GaussianExpressionPrior(mean: Double, sdev: Double) extends DistributionEvaluator[RenderParameter] {

    override def logValue(rps: RenderParameter): Double = {
      val expressCoeff = rps.momo.expression
      if (expressCoeff.isEmpty) {
        0.0
      } else {
        val dDiff = expressCoeff.foldLeft(0.0) { (z: Double, v: Double) => z + math.pow(v - mean, 2) / sdev / sdev }
        val dNorm = -0.5 * expressCoeff.size * math.log(2 * math.Pi) - expressCoeff.size * math.log(sdev)
        dNorm - 0.5 * dDiff
      }
    }

  }

}
