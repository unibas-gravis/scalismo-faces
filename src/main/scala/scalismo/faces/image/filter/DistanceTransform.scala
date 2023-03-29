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
import scalismo.geometry.{_1D, Point}
import scalismo.sampling.DistributionEvaluator

/** calculate distance transforms in the image */
object DistanceTransform {

  /** euclidian distance transform, uses algorithm of Felzenszwalb */
  def euclidian(image: PixelImage[Boolean]): PixelImage[Double] = {
    // initial distance image
    val softImage = image.map { x => if (x) 0.0 else Double.NegativeInfinity }

    // squared distance evaluator
    val evalSqDist = new DistributionEvaluator[Point[_1D]] {
      override def logValue(sample: Point[_1D]): Double = -math.pow(sample.x, 2)
    }

    // calculate with max convolution
    val negSqDistance = GeneralMaxConvolution.separable2D(softImage, evalSqDist)

    // result of MaxConvolution is negative squared distance
    negSqDistance.map { x => math.sqrt(-x) }
  }

  /** calculate the signed distance transform: inside object with negative distance to border */
  def signedEuclidian(image: PixelImage[Boolean]): PixelImage[Double] = {
    // find outside distance transform
    val outsideDistance = euclidian(image)
    // find inside distance transform
    val insideDistance = euclidian(image.map { !_ })
    // signed distance: negative inside, positive outside
    outsideDistance.zip(insideDistance).map { case (out, in) =>
      if (out > 0.0) out else -in
    }
  }
}
