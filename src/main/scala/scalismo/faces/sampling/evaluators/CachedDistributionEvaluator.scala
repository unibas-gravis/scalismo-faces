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

import scalismo.sampling.DistributionEvaluator
import scalismo.utils.Memoize

/**
 * cache a distribution evaluator's logValue -- Warning: make sure your evaluator does not depend on any side-effects
 */
class CachedDistributionEvaluator[A](evaluator: DistributionEvaluator[A], cacheSize: Int)
    extends DistributionEvaluator[A] {
  private val cachedLogValue = Memoize(evaluator.logValue, cacheSize)

  override def logValue(sample: A): Double = cachedLogValue(sample)
}

object CachedDistributionEvaluator {
  def apply[A](evaluator: DistributionEvaluator[A], cacheSize: Int) =
    new CachedDistributionEvaluator[A](evaluator, cacheSize)

  /** implicit pimping */
  object implicits {
    implicit class RichEvaluator[A](evaluator: DistributionEvaluator[A]) {
      def cached(cacheSize: Int): CachedDistributionEvaluator[A] = CachedDistributionEvaluator(evaluator, cacheSize)
    }
  }
}
