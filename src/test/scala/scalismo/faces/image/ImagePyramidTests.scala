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
import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.AccessMode.Strict
import scalismo.faces.image.pyramid.{GaussPyramid, LaplacePyramid}

import scala.reflect.ClassTag

class ImagePyramidTests extends FacesTestSuite {

  def compareImagesApproximately[A: ClassTag](lhs: PixelImage[A], rhs: PixelImage[A], threshold: Double = 1.0e-6)(implicit ops: ColorSpaceOperations[A]): Unit = {
    lhs.values.zip(rhs.values).foreach{ pixels =>
      val l = pixels._1
      val r = pixels._2
      ops.normSq(ops.add(l,ops.scale(r,-1.0))) should be < threshold
    }
  }

  def doubleImages = Seq(
    chessBoard(white = randomDouble, black = randomDouble),
    chessBoard(64, 8, randomDouble, randomDouble),
    chessBoardWithMissingPixels(white = randomDouble, black = randomDouble, missing = randomDouble),
    chessBoardWithMissingPixels(64, 8, Seq((5, 9), (17, 29)), randomDouble, randomDouble, randomDouble),
    peakImage(peakColor = randomDouble, fillColor = randomDouble),
    peakImage(36, 28, peakColor = randomDouble, fillColor = randomDouble),
    borderImage(borderColor = randomDouble, fillColor = randomDouble),
    borderImage(36, 28, 8, borderColor = randomDouble, fillColor = randomDouble)
  )

  def rgbImages = Seq(
    chessBoard(white = randomRGB, black = randomRGB),
    chessBoard(64, 8, randomRGB, randomRGB),
    chessBoardWithMissingPixels(white = randomRGB, black = randomRGB, missing = randomRGB),
    chessBoardWithMissingPixels(64, 8, Seq((5, 9), (17, 29)), randomRGB, randomRGB, randomRGB),
    peakImage(peakColor = randomRGB, fillColor = randomRGB),
    peakImage(36, 28, peakColor = randomRGB, fillColor = randomRGB),
    borderImage(borderColor = randomRGB, fillColor = randomRGB),
    borderImage(36, 28, 8, borderColor = randomRGB, fillColor = randomRGB)
  )

  def rgbaImages = Seq(
    chessBoard(white = randomRGBA, black = randomRGBA),
    chessBoard(64, 8, randomRGBA, randomRGBA),
    chessBoardWithMissingPixels(white = randomRGBA, black = randomRGBA, missing = randomRGBA),
    chessBoardWithMissingPixels(64, 8, Seq((5, 9), (17, 29)), randomRGBA, randomRGBA, randomRGBA),
    peakImage(peakColor = randomRGBA, fillColor = randomRGBA),
    peakImage(36, 28, peakColor = randomRGBA, fillColor = randomRGBA),
    borderImage(borderColor = randomRGBA, fillColor = randomRGBA),
    borderImage(36, 28, 8, borderColor = randomRGBA, fillColor = randomRGBA)
  )

  describe("A Gaussian Pyramid") {

    it("can be constructed for doubles.") {
      val images = doubleImages
      images.foreach(GaussPyramid(_))
    }

    it("can be constructed with RGB") {
      val images = rgbImages
      images.foreach(GaussPyramid(_))
    }

    it("can be constructed with RGBA") {
      val images = rgbaImages
      images.foreach(GaussPyramid(_))
    }

    it("calculates the correct number of prime factors two") {

      def generateNumbers(n: Int, N: Int) = {
        val primeNumbersWihtoutTwo = Seq(3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
        val numberOfPrimes = primeNumbersWihtoutTwo.size
        val partial = for (i <- 0 until N) yield {
          val numberOfFactors = rnd.scalaRandom.nextInt(10)
          val factors = for (j <- 0 until numberOfFactors) yield {
            val idx = rnd.scalaRandom.nextInt(numberOfPrimes)
            primeNumbersWihtoutTwo(idx)
          }
          factors.product
        }
        partial.map(_ * math.pow(2, n).toInt)
      }

      for (n <- 0 until 20) {
        val numbers = generateNumbers(n, 20)
        numbers.foreach { num =>
          GaussPyramid.findNumberOfTwoInPrimFactorDecomposition(num) shouldBe n
        }
      }
    }

  }

  describe("A Laplacian Pyramid") {

    it("can be constructed for doubles.") {
      val images = doubleImages
      images.foreach(LaplacePyramid(_))
    }

    it("can be constructed with RGB") {
      val images = rgbImages
      images.foreach(LaplacePyramid(_))
    }

    it("can be constructed with RGBA") {
      val images = rgbaImages
      images.foreach(LaplacePyramid(_))
    }

    it("build the correct amount of images") {
      val gpy = doubleImages.map(GaussPyramid(_))
      gpy.foreach { py =>
        val lpy = new LaplacePyramid(py, LaplacePyramid.expand[Double])
        lpy.levels shouldBe py.levels
      }
    }

    it("keeps the last level correctly") {
      val gpy = doubleImages.map(GaussPyramid(_))
      gpy.foreach { py =>
        val lpy = new LaplacePyramid(py, LaplacePyramid.expand[Double])
        lpy.level.last shouldBe py.level.last
      }
    }

    it("can reconstruct the original image without significant loss of information.") {
      doubleImages.foreach { img =>
        compareImagesApproximately(img, LaplacePyramid(img).reconstruct)
      }

      rgbImages.foreach { img =>
        compareImagesApproximately(img, LaplacePyramid(img).reconstruct)
      }

      rgbaImages.foreach { img =>
        compareImagesApproximately(img, LaplacePyramid(img).reconstruct)
      }
    }

  }

  def chessBoard[A: ClassTag](widthTotal: Int = 64, widthSquares: Int = 8, white: A, black: A): PixelImage[A] = {
    PixelImage(widthTotal, widthTotal, (x, y) => {
      val X = (x / widthSquares) % 2 == 0
      val Y = (y / widthSquares) % 2 == 0
      if (X && Y || !X && !Y) black
      else white
    }).withAccessMode(Strict())
  }

  def chessBoardWithMissingPixels[A: ClassTag](widthTotal: Int = 64, widthSquares: Int = 8, coords: Seq[(Int, Int)] = Seq((15, 17), (23, 31)), white: A, black: A, missing: A): PixelImage[A] = {
    PixelImage(widthTotal, widthTotal, (x, y) => {
      val X = (x / widthSquares) % 2 == 0
      val Y = (y / widthSquares) % 2 == 0
      if (coords.contains((X, Y))) missing
      else if (X && Y || !X && !Y) black
      else white
    }).withAccessMode(Strict())
  }

  def peakImage[A: ClassTag](width: Int = 64, height: Int = 64, x: Int = 16, y: Int = 16, peakColor: A, fillColor: A): PixelImage[A] = {
    require(x >= 0 && y >= 0 && x < width && y < height, s"the coordinates of the peak (${x}/${y}) need to be inside the rect(0,0,${width},${height}")
    PixelImage(width, height, (X, Y) => if (X == x && Y == y) peakColor else fillColor).withAccessMode(Strict())
  }

  def borderImage[A: ClassTag](width: Int = 64, height: Int = 64, border: Int = 16, borderColor: A, fillColor: A): PixelImage[A] = {
    require(2 * border < width && 2 * border < height, "border can not be larger than min(width/2,height/2)")
    PixelImage(width, height, (x, y) => if (x >= border && y >= border && x < (width - border) && y < (height - border)) borderColor else fillColor).withAccessMode(Strict())
  }

}
