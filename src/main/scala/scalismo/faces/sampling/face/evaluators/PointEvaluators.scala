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

import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.PairEvaluator
import scalismo.geometry._

import scala.math._

object PointEvaluators {
  case class IsotropicGaussianPointEvaluator[D <: Dim](sdev: Double)(implicit ndSpace: NDSpace[D]) extends PairEvaluator[Point[D]] {

    private val d = ndSpace.dimensionality
    val normalizer: Double = -0.5 * d * log(2 * Pi) - d * log(sdev)

    override def logValue(first: Point[D], second: Point[D]): Double = {
      val d2 = (second - first).norm2
      normalizer - 0.5f * d2 / sdev / sdev
    }
  }

  case class ConstantPointEvaluator[D <: Dim](value: Double) extends DistributionEvaluator[Point[D]] {
    override def logValue(sample: Point[D]): Double = value
  }

}
