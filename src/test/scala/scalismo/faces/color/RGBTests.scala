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

import java.awt.Color

import scalismo.faces.FacesTestSuite

class RGBTests extends FacesTestSuite {

  describe("RGB") {
    it("can be converted to Java AWT Color") {
      val awtColor = Color.CYAN
      RGB(0, 1, 1).toAWTColor shouldBe awtColor
    }

    it("can be created from a Java AWT Color") {
      val awtColor = Color.CYAN
      RGB(awtColor) shouldBe RGB(0, 1, 1)
    }

    it("supports a roundtrip conversion to/from AWT Color") {
      val color = randomRGB
      RGB(color.toAWTColor).toAWTColor shouldBe color.toAWTColor
    }
  }

  describe("RGBA") {
    it("can be converted to Java AWT Color") {
      val awtColor = Color.CYAN
      RGBA(0, 1, 1).toAWTColor shouldBe awtColor
    }

    it("can be created from a Java AWT Color") {
      val awtColor = Color.CYAN
      RGBA(awtColor) shouldBe RGBA(0, 1, 1)
    }

    it("supports a roundtrip conversion to/from AWT Color") {
      val color = randomRGBA
      RGBA(color.toAWTColor).toAWTColor shouldBe color.toAWTColor
    }
  }
}
