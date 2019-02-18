/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.color

import scalismo.color.{RGB, RGBA}
import scalismo.faces.FacesTestSuite

class RGBTests extends FacesTestSuite {

  describe("RGB") {
    it("AWT color can be converted to RGB and back") {
      val color = randomRGB.toAWTColor
      RGB(color).toAWTColor shouldBe color
    }
  }

  describe("RGBA") {
    it("AWT color can be converted to RGBA and back") {
      val color = randomRGBA.toAWTColor
      RGBA(color).toAWTColor shouldBe color
    }
  }
}
