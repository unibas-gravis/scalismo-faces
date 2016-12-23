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
import scalismo.faces.color.ColorSpaceOperations.implicits._
import scalismo.faces.image.PixelImage.implicits._

class ImageOperatorsTest extends FacesTestSuite {

  val w = 7
  val h = 11
  val r = 4

  describe("A PixelImage[A] for A=RGB") {
    val i1 = randomImage(w, h)
    val i2 = randomImage(w, h)
    val f = PixelImage(w, h, (x, y) => rnd.scalaRandom.nextDouble())

    it("supports +") {
      val i3 = i1 + i2
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        i1(x, y) + i2(x, y) shouldBe i3(x, y)
      }
    }

    it("supports -") {
      val i3 = i1 - i2
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        i1(x, y) - i2(x, y) shouldBe i3(x, y)
      }
    }

    it("supports x (element-wise multiplication), multiply") {
      val i3 = i1 x i2
      val i4 = i1 multiply i2
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        i1(x, y) x i2(x, y) shouldBe i3(x, y)
        i1(x, y) multiply i2(x, y) shouldBe i3(x, y)
      }
    }

    it("supports * (scale)") {
      val i3 = i1 * f
      val i4 = f *: i1
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        i1(x, y) * f(x, y) shouldBe i3(x, y)
        f(x, y) *: i1(x, y) shouldBe i4(x, y)
      }
    }

    it("supports / (scale)") {
      val i3 = i1 / f
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        i1(x, y) / f(x, y) shouldBe i3(x, y)
      }
    }

    it("supports normSq") {
      val n = i1.normSq
      i1.values.map(_.normSq).sum shouldBe n
    }

    it("supports dot") {
      val d = i1 dot i2
      i1.zip(i2).values.map { case (p1, p2) => p1 dot p2 }.sum shouldBe d
    }

    it("supports unary -") {
      val i3 = -i1
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        -i1(x, y) shouldBe i3(x, y)
      }
    }
  }
}
