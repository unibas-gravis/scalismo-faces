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

import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.faces.parameters.RenderParameter

/** landmarks renderer, controlled by RenderParameter, used for fitting */
trait ParametricLandmarksRenderer {
  /** render landmark given by lmId */
  def renderLandmark(lmId: String, parameter: RenderParameter): Option[TLMSLandmark2D]

  /** check if landmark id is known */
  def hasLandmarkId(lmId: String): Boolean

  /** list of all known landmarks */
  def allLandmarkIds: IndexedSeq[String]
}
