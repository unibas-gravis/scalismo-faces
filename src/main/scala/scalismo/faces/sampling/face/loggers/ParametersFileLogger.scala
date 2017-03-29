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

import java.io.File

import scalismo.faces.io.RenderParameterIO
import scalismo.faces.parameters.RenderParameter
import scalismo.sampling.loggers.AcceptRejectLogger
import scalismo.sampling.{DistributionEvaluator, ProposalGenerator}

/** log parameter files */
class ParametersFileLogger(path: File, fileNamePrefix: String) extends AcceptRejectLogger[RenderParameter] {
  private var counter = 0

  override def accept(current: RenderParameter, sample: RenderParameter, generator: ProposalGenerator[RenderParameter], evaluator: DistributionEvaluator[RenderParameter]): Unit = {
    RenderParameterIO.write(sample, new File(path, f"$fileNamePrefix$counter%08d.rps"))
    counter += 1
  }

  override def reject(current: RenderParameter, sample: RenderParameter, generator: ProposalGenerator[RenderParameter], evaluator: DistributionEvaluator[RenderParameter]): Unit = {
    counter += 1
  }
}

object ParametersFileLogger {
  def apply(path: File, fileNamePrefix: String) = new ParametersFileLogger(path, fileNamePrefix)
}

