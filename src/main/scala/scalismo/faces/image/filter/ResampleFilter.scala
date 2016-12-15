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

package scalismo.faces.image.filter

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.{ColumnMajorImageDomain, InterpolationKernel, PixelImage, RowMajorImageDomain}

import scala.reflect.ClassTag

/** filter to resample a discrete image, uses a kernel filter to interpolate or decimate (filtered down-sampling) */
case class ResampleFilter[A: ClassTag](cols: Int, rows: Int, kernel: InterpolationKernel)(implicit ops: ColorSpaceOperations[A]) extends ImageFilter[A, A] {
  require(cols > 0 && rows > 0, s"cannot resample to new size of 0: ($cols, $rows)")

  /** image resampling with a proper convolution kernel, can interpolate and decimate */
  override def filter(image: PixelImage[A]): PixelImage[A] = {
    ResampleFilter.resampleImage(image, cols, rows, kernel)
  }
}

object ResampleFilter {
  /** resample a discrete image, uses a kernel filter to interpolate or decimate (filtered down-sampling) */
  def resampleImage[@specialized(Double, Float, Int, Boolean) A: ClassTag](image: PixelImage[A], cols: Int, rows: Int, kernel: InterpolationKernel)(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    require(cols > 0 && rows > 0, s"cannot resample to new size of 0: ($cols, $rows)")
    require(image.width > 0 && image.height > 0, s"cannot resample image size of 0: (${image.width}, ${image.height})")
    import ColorSpaceOperations.implicits._

    val width = image.width
    val height = image.height

    // scale factor
    val scaleW = width.toDouble / cols
    val scaleH = height.toDouble / rows

    // kernel is only scaled in decimation case
    val kScaleW = math.max(1.0, scaleW)
    val kScaleH = math.max(1.0, scaleH)

    // kernel
    val rx = kernel.radius * kScaleW

    // row filter for the image
    def filter1D(cols: Int, scale: Double, kScale: Double) = new ImageFilter[A, A] {
      override def filter(image: PixelImage[A]): PixelImage[A] = {
        def f(i: Int, j: Int): A = {
          // center point for access in this continuous image
          val x = i + 0.5
          // center point in underlying image grid (i,j)
          val ix = x * scale - 0.5
          // filtering, scaled kernel, spacing of original image
          val left = math.ceil(ix - rx).toInt
          val right = math.floor(ix + rx).toInt
          // do filtering
          var sx: Int = left
          var kSumX: Double = 0.0
          var kvsum: A = ops.zero
          while (sx <= right) {
            val k = kernel((ix - sx) / kScale)
            kSumX += k
            kvsum += image(sx, j) * k
            sx += 1
          }
          kvsum / kSumX
        }
        PixelImage(cols, image.height, f)
      }
    }

    val rowFilter = filter1D(cols, scaleW, kScaleW)
    val colFilter = filter1D(rows, scaleH, kScaleH)

    // do the resampling: choose inner loop based on image domain, buffer the intermediate result (careful with access modes)
    image.domain match {
      case _: ColumnMajorImageDomain => image.transposed.filter(colFilter).transposed.filter(rowFilter)
      case _: RowMajorImageDomain => image.filter(rowFilter).transposed.filter(colFilter).transposed
    }
  }
}
