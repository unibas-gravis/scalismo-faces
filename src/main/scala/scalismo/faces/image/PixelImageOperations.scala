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

import scalismo.color.{ColorSpaceOperations, RGB, RGBA}
import scalismo.common.ComponentRepresentation
import scalismo.faces.utils.LanguageUtilities

import scala.reflect.ClassTag

object PixelImageOperations {

  /**
    * Blends the foreground over the background according to the mask.
    * The foreground is visible where the mask is 1.0. The Background is visible where the mask is 0.0. For other mask
    * values the two images are blended accordingly.
    * @param background visible when mask = 0.0
    * @param foreground visible when mask = 1.0
    * @param mask used for blending the colors
    * @return
    */
  def alphaBlending(background: PixelImage[RGB], foreground: PixelImage[RGB], mask: PixelImage[Double]): PixelImage[RGB] = {
    alphaBlending(background,setAlpha(foreground,mask))
  }

  /**
    * Blends the foreground over the background according to the alpha channel of the foreground.
    * The foreground is visible where its alpha channel is 1.0. The Background is visible when the foreground alpha
    * channel is 0.0. For other values the two images are blended accordingly.
    * @param background visible when alpha channel of foreground = 0.0
    * @param foreground visible when alpha channel = 1.0
    * @return
    */
  def alphaBlending(background: PixelImage[RGB], foreground: PixelImage[RGBA]): PixelImage[RGB] = {
    background.zip(foreground).map{
      case (bg,fg) => bg.blend(fg)
    }
  }

  def applyToRGB(image: PixelImage[RGBA], f: PixelImage[RGB] => PixelImage[RGB]): PixelImage[RGBA] = {
    setAlpha(f(removeAlpha(image)), extractAlpha(image))
  }

  def extractAlpha(image: PixelImage[RGBA]): PixelImage[Double] = {
    image.map((c: RGBA) => c.a)
  }

  def removeAlpha(image: PixelImage[RGBA]): PixelImage[RGB] = {
    image.map((c: RGBA) => c.toRGB)
  }

  def setAlpha(image: PixelImage[RGB], alpha: PixelImage[Double]): PixelImage[RGBA] = {
    require(image.domain.size == alpha.domain.size)
    PixelImage(image.domain, (x, y) => RGBA(image(x, y), alpha(x, y)))
  }

  /// subimage of image, lazy
  def subImage[Pixel](image: PixelImage[Pixel], x: Int, y: Int, width: Int, height: Int): PixelImage[Pixel] = {
    PixelImage.view(width, height, (ix: Int, iy: Int) => image(ix + x, iy + y))
  }

  /// shifted image access, lazy (view)
  def shiftImage[Pixel](image: PixelImage[Pixel], ox: Int, oy: Int): PixelImage[Pixel] = {
    val access = (x: Int, y: Int) => {
      val ix = x + ox
      val iy = y + oy
      image(ix, iy)
    }
    PixelImage.view(image.domain, access)
  }

  /** Calculate variance of pixels in image. */
  def variancePerChannel[Pixel: ClassTag](img: PixelImage[Pixel])(implicit pixOps: ColorSpaceOperations[Pixel]): Pixel = {
    val mu = mean(img)
    val muSquared = pixOps.multiply(mu, mu)
    val muImageSquared = mean(img.map[Pixel](c => pixOps.multiply(c, c)))
    pixOps.add(muImageSquared, pixOps.scale(muSquared, -1.0))
  }

  /**Calculate mean of pixels in image.*/
  def mean[Pixel](img: PixelImage[Pixel])(implicit pixOps: ColorSpaceOperations[Pixel]): Pixel = {
    val sum = img.values.reduce((p, q) => pixOps.add(p, q))
    pixOps.scale(sum, 1.0 / (img.width * img.height))
  }

  /** Concatenate images horizontally */
  def stitchHorizontal[Pixel: ClassTag](images: IndexedSeq[PixelImage[Pixel]]): PixelImage[Pixel] = {
    require(images.nonEmpty, "set of images to stich must not be empty")
    val widths = images.map(_.width)
    val heights = images.map(_.height)
    val totalWidth = widths.sum
    val xOffsets = widths.scanLeft(0)(_ + _)
    val maxHeight = heights.max

    def imageData(x: Int, y: Int): Pixel = {
      val ind = xOffsets.indexWhere(_ > x) - 1
      images(ind)(x - xOffsets(ind), y)
    }

    PixelImage.fromTemplate(images.head, totalWidth, maxHeight, imageData)
  }

  /** inset an image into a larger one, copy patch */
  def insetImage[A: ClassTag](targetImage: PixelImage[A], inset: PixelImage[A], left: Int, top: Int): PixelImage[A] = {
    PixelImage.fromTemplate(targetImage, (x, y) =>
      if (x >= left && x < left + inset.width && y >= top && y < top + inset.height)
        inset(x - left, y - top)
      else
        targetImage(x, y))
  }

  /** inset an image into a larger one, copy patch */
  def insetView[A](targetImage: PixelImage[A], inset: PixelImage[A], left: Int, top: Int): PixelImage[A] = {
    PixelImage.view(targetImage.domain, (x, y) =>
      if (x >= left && x < left + inset.width && y >= top && y < top + inset.height)
        inset(x - left, y - top)
      else
        targetImage(x, y)).withAccessMode(targetImage.accessMode)
  }

  /** pad image with constant values */
  def padImage[A: ClassTag](image: PixelImage[A], targetWidth: Int, targetHeight: Int, value: A, left: Int = 0, top: Int = 0): PixelImage[A] = {
    PixelImage.view(targetWidth, targetHeight, (x, y) => {
      if (x >= left && x < left + image.width && y >= top && y < top + image.height) image(x - left, y - top)
      else value
    }).withAccessMode(image.accessMode)
  }

  /** calculate the L2 norm of an image */
  def imageNorm[Pixel](image: PixelImage[Pixel])(implicit pixOps: ColorSpaceOperations[Pixel]): Double = {
    math.sqrt(imageNormSq(image))
  }

  /** calculate the squared L2 image norm */
  def imageNormSq[A](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): Double = {
    image.values.map(p => ops.normSq(p)).sum
  }

  /** average an image to half its size multiple times */
  def pyramidAverage[A](image: PixelImage[A], levels: Int = 1)(implicit ops: ColorSpaceOperations[A], tag: ClassTag[A]): PixelImage[A] = {
    def shrink2(image: PixelImage[A]): PixelImage[A] = image.resample(image.width / 2, image.height / 2, InterpolationKernel.BilinearKernel)
    LanguageUtilities.iterate(image, levels)(shrink2).resample(image.width, image.height, InterpolationKernel.BilinearKernel)
  }

  /** convert a masked image into an optional image with None outside the mask region */
  def optionalFromMasked[A](image: PixelImage[A], mask: PixelImage[Boolean]): PixelImage[Option[A]] = {
    image.zip(mask).map { case (p, m) => if (m) Some(p) else None }
  }

  /** extract mask from optional pixel value, None is mapped to false */
  def maskFromOption[A](image: PixelImage[Option[A]]): PixelImage[Boolean] = image.map(_.isDefined)

  /** generate an image with patterns of increasing frequency from the the center outwards */
  def chirpImage(width: Int): PixelImage[Double] = {
    PixelImage(width, width, (x: Int, y: Int) => {
      val (dx, dy) = (x - width / 2.0, y - width / 2.0)

      val rSq = dx * dx + dy * dy
      val r = math.sqrt(rSq)
      val km = 1.2 * math.Pi
      val rm = 0.9 * width
      val w = rm / 5.0
      val term1 = math.sin((km * rSq) / (2.0 * rm))
      val term2 = 0.5 * math.tanh((rm - r) / w) + 0.5
      (term1 * term2 + 1.0) / 2.0
    })
  }

  /** extract bounding box of predicate in format: (left, top, right, bottom) */
  def boundingBox[A](image: PixelImage[A], pred: A => Boolean): (Int, Int, Int, Int) = {
    val w = image.width
    val h = image.height
    // sum mask horizontally and vertically (find the number of mask pixels in rows/cols)
    // predicate reduction, projection (with || operator) vertically and horizontally
    val red = (a: Boolean, p: A) => a || pred(p)
    val maskVert = for (x <- 0 until w) yield image.col(x).values.foldLeft(false)(red)
    val maskHor = for (y <- 0 until h) yield image.row(y).values.foldLeft(false)(red)
    // extract all indices where predicate is set
    val defIndicesVert: IndexedSeq[Int] = maskVert.zipWithIndex.collect { case (elem, idx) if elem => idx }
    val defIndicesHor: IndexedSeq[Int] = maskHor.zipWithIndex.collect { case (elem, idx) if elem => idx }
    // find first and last index: bounding box (left, top, right, bottom)
    (defIndicesVert.head, defIndicesHor.head, defIndicesVert.last + 1, defIndicesHor.last + 1)
  }

  /** Interprets the target as a multi channel image and extracts each channel as a separate image. */
  def extractChannels[Pixel](target: PixelImage[Pixel])(implicit vec: ComponentRepresentation[Pixel]): IndexedSeq[PixelImage[Double]] = {
    val targetMC = MultiChannelImageBuffer.vectorize(target)
    for (i <- 0 until vec.size) yield {
      PixelImage(targetMC.width, targetMC.height, (x, y) => targetMC(x, y, i))
    }
  }
  /** Flips the target at the horizontally */
  def flipHorizontal[A](target: PixelImage[A]): PixelImage[A] = {
    PixelImage.view(target.width, target.height, (x, y) => target(target.width - x - 1, y))
  }

  /** Flips  the target at the vertically */
  def flipVertical[A](target: PixelImage[A]): PixelImage[A] = {
    PixelImage.view(target.width, target.height, (x, y) => target(x, target.height - y - 1))
  }


}
