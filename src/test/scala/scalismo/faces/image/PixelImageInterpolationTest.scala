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
import scalismo.faces.image.InterpolationKernel._
import scalismo.faces.image.PixelImage.implicits._
import org.scalatest.PrivateMethodTester
import scalismo.color.RGB

import scala.annotation.tailrec

class PixelImageInterpolationTest extends FacesTestSuite with PrivateMethodTester {
  import scala.language.implicitConversions

  def imageDiff(i1: PixelImage[RGB], i2: PixelImage[RGB]): Double = (i1 - i2).norm

  describe("A PixelImage with interpolation") {
    val w = 100
    val h = 100
    val image = PixelImage(100, 100, (x, y) => randomRGB).withAccessMode(AccessMode.Repeat())

    it("has equivalent interpolate.sample and resample methods") {
      val resampledImage = image.resample(50, 50, BilinearKernel)
      val intsampImage = image.interpolate(BilinearKernel).sample(50, 50)
      resampledImage shouldBe intsampImage
    }

    it("uses filtering on downsampling, image.interpolate.sample is not equivalent with Function2DImage.sample") {
      val resampledImage = image.interpolate(BilinearKernel).sample(50, 50)
      val intsampImage = image.interpolate(BilinearKernel).toFunction2D.sample(50, 50)

      resampledImage shouldNot be(intsampImage)
    }

    it("is consistent when resampled (bilinear)") {
      val resampledImage = image.interpolate(BilinearKernel).sample(100, 100)
      imageDiff(resampledImage, image) should be < 0.01 * w * h
    }

    it("is consistent when resampled 100x (bilinear)") {
      @tailrec def resampleImage(img: PixelImage[RGB], n: Int): PixelImage[RGB] = n match {
        case 0 => img
        case _ => resampleImage(img.withAccessMode(image.accessMode).resample(img.width, img.height, InterpolationKernel.BilinearKernel), n - 1)
      }

      imageDiff(resampleImage(image, 100), image) should be < 0.001 * w * h
    }

    it("is nearly consistent (average difference <1% per pixel) when up-down-sampled 2x (Lanczos, lossy)") {
      @tailrec def upDownSampleImage(img: PixelImage[RGB], n: Int): PixelImage[RGB] = n match {
        case 0 => img
        case _ =>
          val doubleImage = img.withAccessMode(image.accessMode).resample(2 * img.width, 2 * img.height, LanczosKernel(3))
          val doubleHalfImage = doubleImage.withAccessMode(image.accessMode).resample(img.width, img.height, LanczosKernel(3))
          upDownSampleImage(doubleHalfImage, n - 1)
      }

      imageDiff(upDownSampleImage(image, 2), image) should be < 0.01 * w * h
    }

    it("has one example for which it yields the right result (cell-based interpolation: (x=0.5, y=0.5) -> (i=0, j=0))") {
      val data = Array(1.0, 2.0, 2.0, 1.0)
      val pixImage = PixelImage(ColumnMajorImageDomain(2, 2), data).withAccessMode(AccessMode.Repeat())
      val contImage = InterpolatedPixelImage(pixImage, InterpolationKernel.BilinearKernel)
      // x
      contImage(0.5, 0.5) shouldBe 1.0
      contImage(1.5, 0.5) shouldBe 2.0
      contImage(1.0, 0.5) shouldBe 1.5
      // y
      contImage(0.5, 0.5) shouldBe 1.0
      contImage(0.5, 1.5) shouldBe 2.0
      contImage(0.0, 1.0) shouldBe 1.5
    }

    it("allows continuous pixel access: (x=0.0, y=0.0) -> (i=0, j=0)") {
      val data = Array(1.0, 2.0, 2.0, 1.0)
      val pixImage = PixelImage(ColumnMajorImageDomain(2, 2), data).withAccessMode(AccessMode.Repeat())
      val contImage = InterpolatedPixelImage(pixImage, InterpolationKernel.BilinearKernel)
      contImage.atContinuousPixel(0.0, 0.0) shouldBe pixImage(0, 0)
      contImage.atContinuousPixel(1.0, 0.0) shouldBe pixImage(1, 0)
      contImage.atContinuousPixel(1.0, 1.0) shouldBe pixImage(1, 1)
    }

    it("interpolates to nearest neighbours") {
      val data = Array(1.0, 2.0, 2.0, 1.0)
      val pixImage = PixelImage(ColumnMajorImageDomain(2, 2), data).withAccessMode(AccessMode.Repeat())
      val contImage = NearestNeighbourPixelImage(pixImage)

      contImage(0.5, 0.5) shouldBe 1.0
      contImage(1.5, 0.5) shouldBe 2.0
      contImage(0.75, 1.25) shouldBe 2.0
      contImage(1.25, 0.25) shouldBe 2
      contImage(1.0, 1.0) shouldBe 1.0
    }

    it("yields a symmetric image (I==I.t) for a resampling (regression test for access modes in resampling)") {
      val checkers = PixelImage(15, 15, (x,y) => if((x+y)%2==0) 1.0 else 0.0 )
      val resampled = checkers.resample(11, 11, InterpolationKernel.BilinearKernel)
      val diff = resampled.transposed.zip(resampled).map{case(b,c) => math.pow(b - c, 2)}
      diff.values.sum should be < 1e-10
    }

  }
}
