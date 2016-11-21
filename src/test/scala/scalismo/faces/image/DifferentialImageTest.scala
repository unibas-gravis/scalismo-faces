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

package scalismo.faces.image

import scalismo.faces.FacesTestSuite

class DifferentialImageTest extends FacesTestSuite {

  describe("A PixelImage has proper differential operations") {
    it("has a proper gradient x, correlation with (-0.5, 0, 0.5)") {
      val testImg = PixelImage(ColumnMajorImageDomain(6, 1), (0f until 6f by 1f).toArray).withAccessMode(AccessMode.Repeat())
      val gX = PixelImageDifferential.gradX(testImg)
      gX.values.toIndexedSeq should be(IndexedSeq(0.5f, 1f, 1f, 1f, 1f, 0.5f))
    }

    it("has a proper gradient y, correlation with (-0.5, 0, 0.5)") {
      val testImg = PixelImage(ColumnMajorImageDomain(1, 6), (0f until 6f by 1f).toArray).withAccessMode(AccessMode.Repeat())
      val gY = PixelImageDifferential.gradY(testImg)
      gY.values.toIndexedSeq should be(IndexedSeq(0.5f, 1f, 1f, 1f, 1f, 0.5f))
    }

  }
}
