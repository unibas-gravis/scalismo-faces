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

import scalismo.color.RGB
import scalismo.common.ComponentRepresentation
import scalismo.faces.FacesTestSuite
import scalismo.faces.color._
import scalismo.faces.common._

class MultiChannelImageBufferTests extends FacesTestSuite {

  val vectorizer = ComponentRepresentation[RGB]

  val w = 17
  val h = 13
  val d = vectorizer.size

  val r = 4

  val emptyBuffer: MultiChannelImageBuffer = MultiChannelImageBuffer(w, h, d)
  val randomBuffer = MultiChannelImageBuffer(w, h, d, Array.fill(w * h * d)(randomDouble))

  val image: PixelImage[RGB] = randomImageRGB(w, h)

  describe("A MultiChannelImageBuffer") {
    it("has proper dimensions") {
      randomBuffer.width shouldBe w
      randomBuffer.height shouldBe h
      randomBuffer.channels shouldBe d
      randomBuffer.length shouldBe w * h * d
    }

    it("equals itself") {
      randomBuffer shouldBe randomBuffer
    }

    it("equals a copy of itself, copy is not the same object") {
      val clone = randomBuffer.clone()
      clone shouldBe randomBuffer
      clone.eq(randomBuffer) shouldBe false
    }

    it("a copy does not write-through to the original") {
      val clone = emptyBuffer.clone()
      for (i <- 0 until r) {
        val (x, y, c) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h), rnd.scalaRandom.nextInt(d))
        val f = randomDouble
        clone(x, y, c) = f
        clone(x, y, c) shouldBe f
        emptyBuffer(x, y, c) shouldNot be(f)
      }
    }

    it("supports (x, y, c) read and write access with apply/update") {
      for (i <- 0 until r) {
        val (x, y, c) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h), rnd.scalaRandom.nextInt(d))
        val f = randomDouble
        emptyBuffer(x, y, c) = f
        emptyBuffer(x, y, c) shouldBe f
      }
    }

    it("supports channel slice access, read") {
      for (i <- 0 until r) {
        val (x, y) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h))
        val slice = randomBuffer.channelSlice(x, y)
        val data = Array.tabulate(d)(i => randomBuffer(x, y, i))
        slice.sameElements(data) should be(true)
      }
    }

    it("supports channel slice access, cannot write back") {
      for (i <- 0 until r) {
        val (x, y) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h))
        val slice = randomBuffer.channelSlice(x, y)
        // fill with random values
        val randomValues = IndexedSeq.fill(d)(randomDouble)
        for (j <- 0 until d) { slice(j) = randomValues(j) }
        // check new slice read
        val test = randomBuffer.channelSlice(x, y)
        test.toIndexedSeq shouldNot be(randomValues)
      }
    }

    it("supports channel slice write access with updateChannelSlice") {
      for (i <- 0 until r) {
        val (x, y) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h))
        val slice = randomBuffer.channelSlice(x, y)
        // fill with random values
        val randomValues = Array.fill(d)(randomDouble)
        randomBuffer.updateChannelSlice(x, y, randomValues)
        // check new slice read
        val test = randomBuffer.channelSlice(x, y)
        test.toIndexedSeq shouldBe randomValues.toIndexedSeq
      }
    }

    it("can be created from an image [RGB]") {
      val mcb = MultiChannelImageBuffer.vectorize(image)
      for (i <- 0 until r) {
        val (x, y, c) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h), rnd.scalaRandom.nextInt(d))
        mcb(x, y, c) shouldBe vectorizer.component(image(x, y), c)
      }
    }

    it("preserves image in conversion round-trip") {
      MultiChannelImageBuffer.vectorize(image).toImage[RGB] shouldBe image
    }

    it("does not write through to original image") {
      val mcb = MultiChannelImageBuffer.vectorize(image)
      for (i <- 0 until r) {
        val (x, y) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h))
        val original = image(x, y)
        // update value in buffer
        for (c <- 0 until d)
          mcb(x, y, c) = randomDouble
        // check for modification
        image(x, y) shouldBe original
      }
    }

    it("does not write through to converted image") {
      val convImage = randomBuffer.toImage[RGB]
      for (i <- 0 until r) {
        val (x, y) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h))
        val original = convImage(x, y)
        // update value in buffer
        for (c <- 0 until d) {
          randomBuffer(x, y, c) = randomDouble
        }
        // check for modification
        convImage(x, y) shouldBe original
      }
    }

    it("does write through to image view") {
      val convImage = randomBuffer.toUnsafeImage[RGB]
      for (i <- 0 until r) {
        val (x, y) = (rnd.scalaRandom.nextInt(w), rnd.scalaRandom.nextInt(h))
        val original = convImage(x, y)
        // update value in buffer
        val value = for (c <- 0 until d) yield {
          val f = randomDouble
          randomBuffer(x, y, c) = f
          f
        }
        // check for modification
        convImage(x, y) shouldBe vectorizer.fromArray(value.toArray)
      }
    }
  }
}
