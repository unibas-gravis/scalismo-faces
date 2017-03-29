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

package scalismo.faces.landmarks

import scalismo.geometry._

/** 2d landmark in image */
case class TLMSLandmark2D(id: String, point: Point[_2D], visible: Boolean) {
  def toLandmark = Landmark(id, point, None, None)
}

/** 3D landmark */
case class TLMSLandmark3D(id: String, point: Point[_3D], visible: Boolean) {
  def toLandmark = Landmark(id, point, None, None)
}
