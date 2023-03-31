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
import scalismo.sampling.DistributionEvaluator

class CachedDistributionEvaluatorTest extends FacesTestSuite {

  private val underlyingEval: DistributionEvaluator[Double] = new DistributionEvaluator[Double] {
    override def logValue(sample: Double): Double = -sample
  }

  describe("CachedDistributionEvaluatorTest") {

    it("should yield the underlying evaluator's logValue") {
      val cachedEval = CachedDistributionEvaluator(underlyingEval, 1)
      cachedEval.logValue(-5.2) shouldBe underlyingEval.logValue(-5.2)
    }

    it("should cache the underlying evaluator's logValue and not execute a side-effect a second time") {
      var sideEffectCounter = 0
      val evalWithSideEffect: DistributionEvaluator[Double] = new DistributionEvaluator[Double] {
        override def logValue(sample: Double): Double = {
          sideEffectCounter += 1
          -sample
        }
      }

      val cachedEval = CachedDistributionEvaluator(evalWithSideEffect, 2)

      def shouldBeCached(value: Double): Unit = {
        sideEffectCounter = 0
        cachedEval.logValue(value) shouldBe evalWithSideEffect.logValue(value)
        sideEffectCounter shouldBe 1 // only 1 from underlying
      }

      def shouldNotBeCached(value: Double): Unit = {
        sideEffectCounter = 0
        cachedEval.logValue(value) shouldBe evalWithSideEffect.logValue(value)
        sideEffectCounter shouldBe 2 // 2 calls: cached and underlying
      }

      // tests
      shouldNotBeCached(1.0)
      shouldBeCached(1.0)
    }

    it("should keep its cache at the specified size limit") {
      var sideEffectCounter = 0
      val evalWithSideEffect: DistributionEvaluator[Double] = new DistributionEvaluator[Double] {
        override def logValue(sample: Double): Double = {
          sideEffectCounter += 1
          -sample
        }
      }

      val cachedEval = CachedDistributionEvaluator(evalWithSideEffect, 2)

      def shouldBeCached(value: Double): Unit = {
        sideEffectCounter = 0
        cachedEval.logValue(value) shouldBe evalWithSideEffect.logValue(value)
        sideEffectCounter shouldBe 1 // only 1 from underlying
      }

      def shouldNotBeCached(value: Double): Unit = {
        sideEffectCounter = 0
        cachedEval.logValue(value) shouldBe evalWithSideEffect.logValue(value)
        sideEffectCounter shouldBe 2 // 2 calls: cached and underlying
      }
      // first value
      shouldNotBeCached(1.0)
      shouldBeCached(1.0)
      // second value: first and second in cache
      shouldNotBeCached(2.0)
      shouldBeCached(2.0)
      shouldBeCached(1.0)
      // third value: second and third in cache, first removed
      shouldNotBeCached(3.0)
      shouldBeCached(3.0)
      shouldBeCached(2.0)
      shouldNotBeCached(1.0)
    }

    it("can be constructed using implicit notation") {
      import CachedDistributionEvaluator.implicits._
      underlyingEval.cached(2) shouldBe a[CachedDistributionEvaluator[_]]
    }
  }
}
