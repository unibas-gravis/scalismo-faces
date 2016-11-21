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
import scalismo.faces.color._
import scalismo.faces.image.AccessMode._
import scalismo.faces.image.PixelImage.implicits._
import org.scalatest.PrivateMethodTester

class PixelImageAccessModes extends FacesTestSuite with PrivateMethodTester {

  def imageDiff(i1: PixelImage[RGB], i2: PixelImage[RGB]): Double = (i1 - i2).norm

  describe("A PixelImage with access modes") {
    val image = PixelImage(10, 10, (x, y) => randomRGB)

    it("supports strict access mode") {
      val direct = image.withAccessMode(Strict())
      direct(0, 2) shouldBe image.valueAt(0, 2)
      direct(3, 7) shouldBe image.valueAt(3, 7)
      intercept[Exception](direct(-1, 1))
      intercept[Exception](direct(1, -1))
      intercept[Exception](direct(10, 1))
      intercept[Exception](direct(1, 10))
    }

    it("supports repeated access") {
      val repeat = image.withAccessMode(Repeat())
      repeat(3, 7) shouldBe image.valueAt(3, 7)
      repeat(3, 11) shouldBe image(3, 9)
      repeat(-2, 1) shouldBe image(0, 1)
      repeat(-200, 1) shouldBe image(0, 1)
      repeat(-2, 1) shouldBe image(0, 1)
    }

    it("supports mirrored access") {
      val mirror = image.withAccessMode(Mirror())
      mirror(3, 7) shouldBe image.valueAt(3, 7)
      mirror(3, 10) shouldBe image(3, 9)
      mirror(-2, 1) shouldBe image(1, 1)
      mirror(-200, 1) shouldBe image(0, 1)
      mirror(7, 10) shouldBe image(7, 9)
      mirror(7, -2) shouldBe image(7, 1)
    }

    it("supports periodic access") {
      val periodic = image.withAccessMode(Periodic())
      periodic(3, 7) shouldBe image.valueAt(3, 7)
      periodic(3, 10) shouldBe image(3, 0)
      periodic(-2, 1) shouldBe image(8, 1)
      periodic(9, -3) shouldBe image(9, 7)
      periodic(-200, 1) shouldBe image(0, 1)
    }

    it("supports padded access") {
      val padded = image.withAccessMode(Padded(RGB.White))
      padded(3, 7) shouldBe image.valueAt(3, 7)
      padded(3, 10) shouldBe RGB.White
      padded(-2, 1) shouldBe RGB.White
      padded(-200, 1) shouldBe RGB.White
    }

    it("buffering keeps access mode active") {
      val buffered = image.withAccessMode(Padded(RGB.White))
      buffered(-2, 1) shouldBe RGB.White
    }

    it("buffering with an IndexedSeq keep access mode active") {
      val buffered = image.withAccessMode(Padded(RGB.White)).buffer
      buffered(-2, 1) shouldBe RGB.White
    }
  }
}
