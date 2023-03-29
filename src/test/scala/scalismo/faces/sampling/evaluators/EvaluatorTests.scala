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

import scalismo.faces.FacesTestSuite
import scalismo.sampling.evaluators.GaussianEvaluator

class EvaluatorTests extends FacesTestSuite {

  describe("The LogNormalDistribution") {
    it("returns the same density value as a Gaussian of the log") {
      val x = 0.5 + math.abs(rnd.scalaRandom.nextGaussian())
      val dlogN = LogNormalDistribution.logDensity(x, 0.0, 1.0)
      val dGauss = GaussianEvaluator.logDensity(math.log(x), 0.0, 1.0)
      dlogN shouldBe dGauss +- 1e-5
    }

    it("returns the same value if evaluated as a factor around 1.0 or as value around the mean (mean can be shifted)") {
      val dLogM = LogNormalDistribution.logDensity(2.0, math.log(1.5), math.log(1.5))
      val dLogZ = LogNormalDistribution.logDensity(2.0 / 1.5, 0.0, math.log(1.5))
      dLogM shouldBe dLogZ +- 1e-5
    }
  }
}
