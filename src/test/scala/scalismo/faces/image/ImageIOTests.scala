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

import java.io._

import scalismo.faces.FacesTestSuite
import scalismo.faces.color.ColorSpaceOperations.implicits._
import scalismo.faces.color._
import scalismo.faces.io.PixelImageIO
import scalismo.faces.utils.LanguageUtilities

import scala.reflect.ClassTag

class ImageIOTests extends FacesTestSuite {

  /** number of cycles to test*/
  val repetitions = 2
  /** reconstruction tolerance, IO is only 8 bit! */
  val tolerance = math.sqrt(4*math.pow(1.0/255.0,2.0)) // 8 bit write/read

  val img = randomImage(37, 67)
  val imgG = img.map(_.gray)
  val imgA = img.map(RGBA(_))

  /** evaluate difference of repeated identity transforms */
  def diffOfIdentityTransform[A: ClassTag](image: PixelImage[A], identityTransform: PixelImage[A] => PixelImage[A])
                                          (implicit converter: BufferedImageConverter[A], ops: ColorSpaceOperations[A])
  : Double = {
    val wrImage: PixelImage[A] = repeatIdentityTransformation(image,identityTransform)
    val diff = PixelImage(image.width, image.height, (x, y) => image(x, y) - wrImage(x, y))
    math.sqrt(diff.values.map(ops.normSq).max)
  }

  /** evaluate difference of repeated identity transforms */
  def diffOfSRGBIdentityTransform(image: PixelImage[SRGB], identityTransform: PixelImage[SRGB] => PixelImage[SRGB])
                                 (implicit converter: BufferedImageConverter[SRGB])
  : Double = {
    val wrImage: PixelImage[SRGB] = repeatIdentityTransformation(image,identityTransform)
    val diff = PixelImage(image.width, image.height, (x, y) => math.pow(image(x, y).r - wrImage(x, y).r, 2) +
      math.pow(image(x, y).g - wrImage(x, y).g, 2) +
      math.pow(image(x, y).b - wrImage(x, y).b, 2))
    math.sqrt(diff.values.max)
  }

  /** evaluate difference of repeated identity transforms */
  def diffOfSRGBAIdentityTransform(image: PixelImage[SRGBA], identityTransform: PixelImage[SRGBA] => PixelImage[SRGBA])
                                  (implicit converter: BufferedImageConverter[SRGBA])
  : Double = {
    val wrImage: PixelImage[SRGBA] = repeatIdentityTransformation(image,identityTransform)
    val diff = PixelImage(image.width, image.height, (x, y) => math.pow(image(x, y).r - wrImage(x, y).r, 2) + math.pow(image(x, y).g - wrImage(x, y).g, 2) + math.pow(image(x, y).b - wrImage(x, y).b, 2) + math.pow(image(x, y).a - wrImage(x, y).a, 2))
    math.sqrt(diff.values.max)
  }

  /** execute repeated identity transforms */
  def repeatIdentityTransformation[A](image: PixelImage[A], identity: PixelImage[A] => PixelImage[A])
                                    (implicit converter: BufferedImageConverter[A])
  : PixelImage[A] = LanguageUtilities.iterate(image, repetitions)(identity)

  /** perform a write-read cycle for an image */
  def writeReadCycle[A](img: PixelImage[A])(implicit conv: BufferedImageConverter[A]): PixelImage[A] = {
    val f = File.createTempFile("faces-scala-iotest", ".png")
    f.deleteOnExit()
    PixelImageIO.write[A](img, f).get
    PixelImageIO.read[A](f).get
  }

  /** perform a write-read cycle for an image */
  def writeReadStreamCycle[A](img: PixelImage[A])(implicit conv: BufferedImageConverter[A]): PixelImage[A] = {
    val os = new ByteArrayOutputStream()
    assert(PixelImageIO.writeToStream[A](img, os).isSuccess)
    val is  = new ByteArrayInputStream(os.toByteArray)
    PixelImageIO.readFromStream[A](is).get
  }

  describe("A random sRGB color image") {
    it("survives a write-read cycle unaltered") {
      diffOfSRGBIdentityTransform(img.map(_.toSRGB),writeReadCycle[SRGB]) should be <= tolerance
    }
  }

  describe("A random sRGBA color image") {
    it("survives a write-read cycle unaltered") {
      diffOfSRGBAIdentityTransform(imgA.map(_.toSRGBA),writeReadCycle[SRGBA]) should be <= tolerance
    }
  }

  describe("A random gray image") {
    it("survives a write-read cycle unaltered") {
      diffOfIdentityTransform(imgG,writeReadCycle[Double]) should be <= tolerance
    }
  }

  describe("The IO-Method") {
    it("writes and reads and image unaltered with streams") {
      diffOfSRGBAIdentityTransform(imgA.map(_.toSRGBA),writeReadStreamCycle[SRGBA]) should be <= tolerance
    }
  }
}
