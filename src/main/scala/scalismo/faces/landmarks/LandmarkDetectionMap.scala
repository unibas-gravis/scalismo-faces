/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.faces.landmarks

import scalismo.faces.image.PixelImage

/** LandmarkDetectionMap contains certainty of detecting a landmark at an image location (log certainty!) */
case class LandmarkDetectionMap(tag: String, logValues: PixelImage[Double]) {

  /** correct detection certainty to respect false positive and false negative rates */
  def correctCertaintyForErrorRates(falsePositiveRate: Double, falseNegativeRate: Double): LandmarkDetectionMap = {
    copy(logValues =
      logValues.map{ logCertainty =>
        math.log(math.exp(logCertainty) * (1.0 - falsePositiveRate - falseNegativeRate) + falseNegativeRate)
      })
  }
}