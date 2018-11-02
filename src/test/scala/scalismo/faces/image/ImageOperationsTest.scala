/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.faces.image

import scalismo.faces.FacesTestSuite
import scalismo.color.RGB

class ImageOperationsTest  extends FacesTestSuite {

  describe("ImageOperations support") {

    it("alpha blending of two images") {
      val bg = PixelImage(PixelImageDomain(2, 2), (x, y) => RGB(1, 0, 0))
      val fg = PixelImage(PixelImageDomain(2, 2), (x, y) => RGB(0, 1, 0))
      val mask = PixelImage(PixelImageDomain(2, 2), (x, y) => (x + y) / 2.0)

      val expected = PixelImage(PixelImageDomain(2, 2), Array(RGB(1, 0, 0), RGB(0.5, 0.5, 0.0), RGB(0.5, 0.5, 0.0), RGB(0.0, 1.0, 0.0)))

      val produced = PixelImageOperations.alphaBlending(bg, fg, mask)

      val diff = PixelImage(produced.width, produced.height, (x, y) => produced(x, y) - expected(x, y))
      math.sqrt(diff.values.map(_.norm).max) should be <= 1.0e-5
    }

  }

}
