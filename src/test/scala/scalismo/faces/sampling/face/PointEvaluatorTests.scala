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

package scalismo.faces.sampling.face

import scalismo.faces.FacesTestSuite
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{Point, _2D}

class PointEvaluatorTests extends FacesTestSuite {

  val origin: Point[_2D] = Point.origin[_2D]

  def randomPoint(distance: Double): Point[_2D] = origin + randomDirection * distance

  describe("PointEvaluators:") {

    it("The isotropic Gaussian evaluator obeys the 2d Gaussian density") {
      val sigma = 1.5
      val dist = 2.0

      val eval = IsotropicGaussianPointEvaluator[_2D](sigma)
      val distEval = eval.toDistributionEvaluator(Point(0.0, 0.0))

      val point = randomPoint(dist * sigma)

      distEval.logValue(point) - math.log(math.Pi * 2) - 2 * math.log(sigma) - 0.5 * dist * dist should be < 1e-4
    }

  }
}
