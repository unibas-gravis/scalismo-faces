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

import scalismo.faces.color.RGB


object PixelImageNormalization {

  /** Normalizes the image such that its values are on the interval [lower, upper]. */
  def normalizeDoubleImageToRange(image: PixelImage[Double], lower: Double, upper: Double): PixelImage[Double] = {
    def normalizer(x: Double) = (x - lower) / (upper - lower)
    if (lower < 0.0 || upper > 1.0)
      image
    else
      image.map(normalizer)
  }

  /** Normalize the value range to the interval [0,1]. */
  def normalizeDoubleImage(image: PixelImage[Double]): PixelImage[Double] = {
    normalizeDoubleImageToRange(image, 0.0, 1.0)
  }

  /** Normalize the image to range ([0,1], [0,1], [0,1]). (Each channel is normalized seperately.) */
  def normalizedRGBPerChannel(img: PixelImage[RGB]): PixelImage[RGB] = {
    val r = normalizeDoubleImage(img.map(_.r))
    val g = normalizeDoubleImage(img.map(_.g))
    val b = normalizeDoubleImage(img.map(_.b))
    PixelImage(img.width, img.height, (x, y) => RGB(r(x, y), g(x, y), b(x, y)))
  }

  /** Expand value range without saturating a channel.
    * Normalizes the image such that the smallest value over all channels is 0 and the largest over all channels is 1. */
  def normalizedRGB(img: PixelImage[RGB]): PixelImage[RGB] = {
    def minmax(img: PixelImage[Double]): (Double, Double) = {
      val arr = img.toArray
      (arr.min, arr.max)
    }

    val imgr = img.map(_.r)
    val imgg = img.map(_.g)
    val imgb = img.map(_.b)

    val (minr, maxr) = minmax(imgr)
    val (ming, maxg) = minmax(imgg)
    val (minb, maxb) = minmax(imgb)
    val min = math.min(minr, math.min(ming, minb))
    val max = math.max(maxr, math.max(maxg, maxb))

    val r = normalizeDoubleImageToRange(imgr, min, max)
    val g = normalizeDoubleImageToRange(imgg, min, max)
    val b = normalizeDoubleImageToRange(imgb, min, max)

    PixelImage(img.width, img.height, (x, y) => RGB(r(x, y), g(x, y), b(x, y)))
  }

  /** Z-Score: Standardizes image to zero mean and unit variance.
    * @return mean, standard deviation, standarizedimage
    */
  def standardizeImage(img: PixelImage[Double]): (Double, Double, PixelImage[Double]) = {
    val mean = PixelImageOperations.mean(img)
    val meanSq = PixelImageOperations.mean(img.map(px=>px*px))
    val std = math.sqrt(meanSq - mean*mean)
    (mean, std, img.map(px => (px - mean)/std))
  }
}
