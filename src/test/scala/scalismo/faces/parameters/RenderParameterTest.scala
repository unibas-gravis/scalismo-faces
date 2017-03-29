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

package scalismo.faces.parameters

import scalismo.faces.FacesTestSuite

class RenderParameterTest extends FacesTestSuite {

  describe("RenderParameters") {
    it("provide a defaultSquare setting with 9 spherical harmonics coefficients") {
      val init = RenderParameter.defaultSquare
      init.environmentMap.coefficients.length shouldBe 9
    }

    it("provide a default setting with 9 spherical harmonics coefficients") {
      val init = RenderParameter.default
      init.environmentMap.coefficients.length shouldBe 9
    }
  }
}
