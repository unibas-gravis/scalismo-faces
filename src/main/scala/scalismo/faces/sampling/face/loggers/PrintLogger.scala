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

package scalismo.faces.sampling.face.loggers

import java.io.PrintStream

import scalismo.sampling.loggers.AcceptRejectLogger
import scalismo.sampling.{DistributionEvaluator, ProposalGenerator}

/** prints current evaluation result and iteration */
class PrintLogger[A](output: PrintStream, prefix: String) extends AcceptRejectLogger[A] {
  private var counter = 0
  override def accept(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = {
    output.println(s"$counter A ${evaluator.logValue(sample)}")
    counter += 1
  }
  override def reject(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = {
    output.println(s"$counter R ${evaluator.logValue(sample)}")
    counter += 1
  }

  def verbose = new VerbosePrintLogger[A](output, prefix)
}

object PrintLogger{
  def apply[A](output: PrintStream = Console.out, prefix: String = "") = new PrintLogger[A](output, prefix)
}

/** prints current evaluation result, iteration number, state, generator and evaluator */
class VerbosePrintLogger[A](output: PrintStream, prefix: String) extends AcceptRejectLogger[A] {
  private var counter = 0
  override def accept(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = {
    output.println(s"$counter A ${evaluator.logValue(sample)} $generator $evaluator")
    counter += 1
  }
  override def reject(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = {
    output.println(s"$counter R ${evaluator.logValue(sample)} $generator $evaluator")
    counter += 1
  }
}
