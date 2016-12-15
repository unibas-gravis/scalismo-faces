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

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.color.ColorSpaceOperations.implicits._
import scalismo.faces.image.filter.ResampleFilter

import scala.reflect.ClassTag

/** continuous access image (create e.g. using PixelImage.interpolate) !!! WARNING: not specialized, specialized version leads to IllegalAccessError on width and height (https://issues.scala-lang.org/browse/SI-4511) */
case class InterpolatedPixelImage[Pixel](image: PixelImage[Pixel],
  kernel: InterpolationKernel)(implicit ops: ColorSpaceOperations[Pixel])
    extends ContinuousPixelImage[Pixel] {

  override val width: Double = image.width
  override val height: Double = image.height

  private val f: (Double, Double) => Pixel = image.domain match {
      case _: ColumnMajorImageDomain => (x, y) => InterpolatedPixelImage.interpolatedImageAccessColFirst(image, x, y, kernel)
      case _: RowMajorImageDomain => (x, y) => InterpolatedPixelImage.interpolateCellBasedRowFirst(image, x, y, kernel)
  }

  /** read the value at the given location, "continuous pixel index" (Double coordinates) */
  override def apply(x: Double, y: Double): Pixel = f(x, y)

  /** apply function to each pixel */
  override def map[B](g: Pixel => B) = MappedContinuousPixelImage(this, g)

  /**
   * resamples the image to produce a new discrete PixelImage - filtered sampling, using the filter of construction
   *
   * @param w Number of samples along x axis (width of image)
   * @param h Number of samples along y axis (height of image)
   */
  override def sample(w: Int, h: Int)(implicit tag: ClassTag[Pixel]): PixelImage[Pixel] = ResampleFilter.resampleImage(image, w, h, kernel)

  /** drop original samples, treat as direct continuous 2d function (removes resampling with filtering, interpolation only) */
  def toFunction2D = Function2DImage(width, height, f)
}

object InterpolatedPixelImage {
  /** direct interpolated image access with kernel-based interpolation, column major: inner loop over y, cell-based interpolation: (0.0, 0.0) is at top left, first sample (0,0) lies at (0.5, 0.5) */
  def interpolatedImageAccessColFirst[@specialized(Double, Float, Int, Boolean) A](image: PixelImage[A], x: Double, y: Double, kernel: InterpolationKernel = InterpolationKernel.BilinearKernel)(implicit ops: ColorSpaceOperations[A]): A = {

    // kernel
    val r = math.max(0.5, kernel.radius)
    // x
    val ix = x - 0.5 // cont. i(x), math.round(ix) determines current cell
    val left = math.ceil(ix - r).toInt // left support end
    val right = math.floor(ix + r).toInt // right support end
    // y
    val jy = y - 0.5 // basic index: determines neighbours for interpolation
    val top = math.ceil(jy - r).toInt
    val bottom = math.floor(jy + r).toInt

    // kernel-based interpolation: convolve with kernel
    var sx = left
    var ksumX = 0.0
    var kvsumX = ops.zero
    while (sx <= right) { // for (sx <- left to right) yield ...
      // y values
      var sy = top
      var kSumY = 0.0
      var kvsumY = ops.zero
      while (sy <= bottom) { // for (sy <- top to bottom) yield ...
        val k = kernel(jy - sy)
        kSumY += k
        kvsumY += image(sx, sy) * k
        sy += 1
      }
      val vy = kvsumY / kSumY

      val k = kernel(ix - sx)
      ksumX += k
      kvsumX += vy * k
      sx += 1
    }
    kvsumX / ksumX
  }

  /** direct interpolated image access with kernel-based interpolation, row major: inner loop over x, cell-based interpolation: (0.0, 0.0) is at top left, first sample (0,0) lies at (0.5, 0.5) */
  def interpolateCellBasedRowFirst[@specialized(Double, Float, Int, Boolean) A](image: PixelImage[A], x: Double, y: Double, kernel: InterpolationKernel = InterpolationKernel.BilinearKernel)(implicit ops: ColorSpaceOperations[A]): A = {

    // kernel
    val r = math.max(0.5, kernel.radius)
    // x
    val ix = x - 0.5 // cont. i(x), math.round(ix) determines current cell
    val left = math.ceil(ix - r).toInt // left support end
    val right = math.floor(ix + r).toInt // right support end
    // y
    val jy = y - 0.5 // basic index: determines neighbours for interpolation
    val top = math.ceil(jy - r).toInt
    val bottom = math.floor(jy + r).toInt

    // kernel-based interpolation: convolve with kernel
    var sy = top
    var kSumY: Double = 0.0
    var kvsumY: A = ops.zero
    while (sy <= bottom) { // for(sy <- top to bottom) yield { interpolate(x, sy) * kernel(jy - sy)}
      var sx = left
      var ksumX = 0.0
      var kvsumX = ops.zero
      while (sx <= right) { // for(sx <- left to right) yield { image(sx, sy) * kernel(ix - sx)}
        val k = kernel(ix - sx)
        ksumX += k
        kvsumX += image(sx, sy) * k
        sx += 1
      }
      val vx = kvsumX / ksumX
      val k = kernel(jy - sy)
      kSumY += k
      kvsumY += vx * k
      sy += 1
    }
    kvsumY / kSumY
  }
}

/** Special case for nearest neighbour interpolated continuous images.*/
case class NearestNeighbourPixelImage[A](image: PixelImage[A]) extends ContinuousPixelImage[A] {
  override val width: Double = image.width
  override val height: Double = image.height

  private def interpolateNearestNeighbour(image: PixelImage[A], x: Double, y: Double): A = {
    val ix = math.round(x - 0.5).toInt // nearest grid cell x
    val iy = math.round(y - 0.5).toInt // nearest grid cell y
    image(ix, iy)
  }

  def apply(x: Double, y: Double): A = interpolateNearestNeighbour(image, x, y)

  /** apply function to each pixel */
  override def map[B](g: (A) => B): ContinuousPixelImage[B] = MappedContinuousPixelImage(this, g)

  /**
    * resamples the image to produce a new discrete PixelImage. Nearest neighbour values are selected during resampling.
    *
    * @param w Number of samples along x axis (width of image)
    * @param h Number of samples along y axis (height of image)
    */
  override def sample(w: Int, h: Int)(implicit tag: ClassTag[A]): PixelImage[A] = nearestNeighbourResample(image, w, h)

  /**
    * Chooses nearest neighbour pixel for pixel value during resampling.
    * */
  def nearestNeighbourResample(image: PixelImage[A], cols: Int, rows: Int)(implicit tag: ClassTag[A]): PixelImage[A] = {
    require(cols > 0 && rows > 0, s"cannot resample to new size of 0: ($cols, $rows)")
    require(image.width > 0 && image.height > 0, s"cannot resample image size of 0: (${image.width}, ${image.height})")

    val width = image.width
    val height = image.height

    // scale factor
    val scaleW = width.toDouble / cols
    val scaleH = height.toDouble / rows

    val domain = image.domain match {
      case _: ColumnMajorImageDomain => ColumnMajorImageDomain(cols, rows)
      case _: RowMajorImageDomain => RowMajorImageDomain(cols, rows)
    }
    val access = (x: Int, y: Int) => interpolateNearestNeighbour(image, (x + 0.5) * scaleW, (y + 0.5) * scaleH)
    PixelImage(domain, access).withAccessMode(AccessMode.Functional(access))
  }
}

/** special class to hold a mapped interpolated pixel image or nearest neighbour pixel image: be careful, apply is mapped "f(apply)" and resample "resample(f)" */
case class MappedContinuousPixelImage[A, B](image: ContinuousPixelImage[A], f: A => B) extends ContinuousPixelImage[B] {
  override val width: Double = image.width
  override val height: Double = image.height

  override def apply(x: Double, y: Double): B = f(image(x, y))

  override def sample(w: Int, h: Int)(implicit tag: ClassTag[B]): PixelImage[B] = image.map(f).sample(w, h)

  /** apply function to each pixel */
  override def map[C](g: (B) => C): ContinuousPixelImage[C] = MappedContinuousPixelImage(image, g compose f)
}