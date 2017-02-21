/*
 * Copyright 2017 University of Basel, Graphics and Vision Research Group
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

import scalismo.faces.image.PixelImage
import scalismo.geometry.{Point, Point1D, _1D}
import scalismo.sampling.DistributionEvaluator

/** calculates general maximum convolutions on images */
object GeneralMaxConvolution {

  /** a separable maximum convolution, e.g. with a Gaussian */
  def separable2D(pixelImage: PixelImage[Double], eval: DistributionEvaluator[Point[_1D]]): PixelImage[Double] = {
    val maxConv = pixelImage.toBuffer

    for (col <- 0 until maxConv.domain.height) {
      val row = pixelImage.row(col)

      val tRow = maxConvolution1D(row.toArray, eval)
      for (x <- 0 until maxConv.domain.width) {
        maxConv(x, col) = tRow(x)
      }
    }

    val tImg = maxConv.toImage

    for (row <- 0 until maxConv.domain.width) {
      val col = tImg.col(row)
      val tCol = maxConvolution1D(col.toArray, eval)
      for (y <- 0 until maxConv.domain.height) {
        maxConv(row, y) = tCol(y)
      }
    }

    maxConv.toImage
  }

  /** 1D max convolution, calculate maximum convolution in 1D */
  def maxConvolution1D(data: Array[Double], eval: DistributionEvaluator[Point[_1D]]): Array[Double] = {

    var maximumsPosition = 0
    var maxConv: Array[Double] = data.clone()

    // keep this line if the local position is often the best one
    maxConv.transform(t => t + eval.logValue(Point1D(0f)))

    // forward pass
    for (i <- data.indices) {
      var mPos = i
      for (lag <- maximumsPosition until i) {
        val t = data(lag) + eval.logValue(Point1D(i - lag))
        if (maxConv(i) < t) {
          maxConv(i) = t
          mPos = lag
        }
      }
      maximumsPosition = mPos
    }

    // backward pass
    for (i <- maximumsPosition to 0 by -1) {
      var mPos = i
      for (lag <- maximumsPosition until i by -1) {
        val t = data(lag) + eval.logValue(Point1D(i - lag))
        if (maxConv(i) < t) {
          maxConv(i) = t
          mPos = lag
        }
      }
      maximumsPosition = mPos
    }

    maxConv
  }
}
