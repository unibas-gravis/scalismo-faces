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
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.loggers.{BestSampleLogger, ChainStateLogger}

/** log best parameter file */
class ParametersFileBestLogger(evaluator: DistributionEvaluator[RenderParameter], fileName: File)
    extends ChainStateLogger[RenderParameter] {
  private val bestLogger = BestSampleLogger(evaluator)

  override def logState(sample: RenderParameter): Unit = {
    bestLogger.logState(sample)
    dumpToFile()
  }

  def dumpToFile(): Unit = {
    val best: Option[RenderParameter] = bestLogger.currentBestSample()
    best.map { b =>
      RenderParameterIO.write(b, fileName)
    }
  }
}

object ParametersFileBestLogger {
  def apply(evaluator: DistributionEvaluator[RenderParameter], fileName: File) =
    new ParametersFileBestLogger(evaluator, fileName)
}
