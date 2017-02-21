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

class PixelImageTests extends FacesTestSuite {

  val w = 13
  val h = 17
  val r = 32

  val fixedImageCM = PixelImage(ColumnMajorImageDomain(2, 3), Array(1f, 2f, 3f, 4f, 5f, 6f))
  val fixedImageRM = PixelImage(RowMajorImageDomain(2, 3), Array(1f, 2f, 3f, 4f, 5f, 6f))

  val rndImage: PixelImage[RGB] = randomImage(w, h)

  describe("A fixed PixelImage") {
    it("has the proper access pattern inside domain (column major)") {
      fixedImageCM(0, 0) shouldBe 1f
      fixedImageCM(1, 0) shouldBe 4f
      fixedImageCM(0, 1) shouldBe 2f
      fixedImageCM(1, 1) shouldBe 5f
      fixedImageCM(0, 2) shouldBe 3f
      fixedImageCM(1, 2) shouldBe 6f
    }

    it("has the proper access pattern inside domain (row major)") {
      fixedImageRM(0, 0) shouldBe 1f
      fixedImageRM(1, 0) shouldBe 2f
      fixedImageRM(0, 1) shouldBe 3f
      fixedImageRM(1, 1) shouldBe 4f
      fixedImageRM(0, 2) shouldBe 5f
      fixedImageRM(1, 2) shouldBe 6f
    }
  }

  describe("A PixelImageDomain") {

    it("supports column major access") {
      val cm = ColumnMajorImageDomain(w, h)
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        val ind = x * h + y
        cm.index(x, y) shouldBe ind
        cm.x(ind) shouldBe x
        cm.y(ind) shouldBe y
      }
    }

    it("supports row major access") {
      val rm = RowMajorImageDomain(w, h)
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        val ind = x + y * w
        rm.index(x, y) shouldBe ind
        rm.x(ind) shouldBe x
        rm.y(ind) shouldBe y
      }
    }

    it("supports major access conversion") {
      ColumnMajorImageDomain(w, h).other shouldBe RowMajorImageDomain(w, h)
      RowMajorImageDomain(w, h).other shouldBe ColumnMajorImageDomain(w, h)
    }

    it("column and row major are transposed versions of each other") {
      val cm = ColumnMajorImageDomain(w, h)
      val rm = cm.other.transpose
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        cm.index(x, y) shouldBe rm.index(y, x)
      }
    }

    it("supports transpose (column major)") {
      val cm = ColumnMajorImageDomain(w, h)
      val t: ColumnMajorImageDomain = cm.transpose
      // index is not preserved on transpose, just gives new ColumnMajorPixelImageDomain with transposed size
      cm.width shouldBe t.height
      cm.height shouldBe t.width
    }

    it("supports transpose (row major)") {
      val rm = RowMajorImageDomain(w, h)
      val t: RowMajorImageDomain = rm.transpose
      rm.width shouldBe t.height
      rm.height shouldBe t.width
    }
  }

  describe("A random PixelImage") {
    val image: PixelImage[RGB] = PixelImage.view(rndImage.domain, (x, y) => rndImage(x, y))

    it("has proper dimensions") {
      image.width shouldBe w
      image.height shouldBe h
      image.length shouldBe w * h
    }

    describe("supports equality based on domain and pixel values") {
      it("is equal to itself") {
        image shouldBe image
      }

      it("is equal to a view of itself") {
        PixelImage.view(image.domain, (x, y) => image(x, y)) shouldBe image
      }

      it("is equal to itself buffered") {
        image.buffer shouldBe image
      }

      it("is not equal to a version with modified pixel values") {
        image.map(p => p * 0.5f) shouldNot be(image)
      }

      it("is not equal to a version with modified domain") {
        image.withDomain(image.domain.other) shouldNot be(image)
      }
    }

    it("can change its domain back and forth") {
      val dimage = image.withDomain(image.domain.other)
      dimage.withDomain(image.domain) shouldBe image
    }

    it("can change its domain") {
      val dimage = image.withDomain(image.domain.other)
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        dimage(x, y) shouldBe image(x, y)
      }
    }

    it("can be transposed") {
      val t = image.transposed
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        image(x, y) shouldBe t(y, x)
      }
    }

    it("support row access") {
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        image.row(y)(x, 0) shouldBe image(x, y)
      }
    }

    it("support column access") {
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        image.col(x)(0, y) shouldBe image(x, y)
      }
    }

    it("can be buffered (preserving all values)") {
      val b = image.buffer
      b.isBuffered shouldBe true
      b.withDomain(b.domain.toColumnMajor).values sameElements image.withDomain(image.domain.toColumnMajor).values shouldBe true
    }

    it("uses a buffer which has the same values as the image") {
      val b = image.buffer
      val vals = image.values
      b.values sameElements image.values shouldBe true
      b.toArray.iterator sameElements image.values shouldBe true
    }

    it("can construct an image from a buffer") {
      val b = image.buffer.toArray
      val i = PixelImage(image.domain, b)
      i shouldBe image
    }

    it("supports toArray for buffered and unbuffered versions") {
      val arrayBuffer = image.buffer.toArray
      val arrayView = PixelImage.view(image.domain, (x, y) => image(x, y)).toArray
      arrayBuffer.deep shouldBe arrayView.deep
    }

    it("supports map") {
      val m = image.map(p => p.gray)
      m.isBuffered shouldBe true
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        m(x, y) shouldBe image(x, y).gray
      }
    }

    it("supports mapLazy") {
      val m = image.mapLazy(p => p.gray)
      m.isBuffered shouldBe false
      for (i <- 0 until r) {
        val x = rnd.scalaRandom.nextInt(w)
        val y = rnd.scalaRandom.nextInt(h)
        m(x, y) shouldBe image(x, y).gray
      }
    }
  }

  describe("A PixelImage") {
    describe("has mapWithIndex with") {
      val image = PixelImage(10, 10, (x, y) => x - y).withAccessMode(AccessMode.Padded(0))

      val summed = image.mapWithIndex((c, x, y) => {
        image(x - 1, y) + image(x + 1, y) + c
      })

      it("proper effect inside image") {
        summed(5, 0) shouldBe 15
        summed(5, 5) shouldBe 15 - 15
      }

      it("proper behavior outside image borders") {
        summed(0, 0) shouldBe 1
        summed(0, 5) shouldBe 1 - 10
        summed(0, -1) shouldBe 0
      }
    }
  }

  describe("PixelImage to/from ImageBuffer") {
    // ensure buffered image
    val blackImage = PixelImage(w, h, (x, y) => RGB.Black).buffer

    // forth and back conversion round-trip
    it("can be converted to ImageBuffer") {
      blackImage.toBuffer.toImage shouldBe blackImage
    }

    val b = blackImage.toBuffer
    // modify buffer
    b(2, 3) = RGB.White
    // convert to image
    val im2 = b.toImage
    // second buffer modification, should not propagate back
    b(3, 2) = RGB.White
    val im3 = b.toImage

    // buffer modified image
    it("buffer can be modified and converted back") {
      im2(2, 3) shouldBe RGB.White
    }
    // second buffer modification
    it("buffer can be modified again and converted back with both changes") {
      im3(2, 3) shouldBe RGB.White
      im3(3, 2) shouldBe RGB.White
    }
    // first buffer modification did not propagate back to original image
    it("first buffer modification does not propagate back to image") {
      blackImage(2, 3) shouldNot be(RGB.White)
    }
    // second buffer modification did not propagate back
    it("second buffer modification does not propagate to image") {
      im2(3, 2) shouldNot be(RGB.White)
    }
  }
}

