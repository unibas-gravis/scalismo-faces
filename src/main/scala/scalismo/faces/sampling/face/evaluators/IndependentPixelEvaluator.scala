/*
 * Copyright University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.sampling.face.evaluators

import breeze.linalg.max
import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.image.PixelImage
import scalismo.faces.sampling.face.evaluators.PixelEvaluators.IsotropicGaussianPixelEvaluator
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.{GaussianEvaluator, PairEvaluator}


/** evaluate each pixel independently, uses alpha channel to determine between foreground and background */
class IndependentPixelEvaluator(val pixelEvaluator: PairEvaluator[RGB], val bgEvaluator: DistributionEvaluator[RGB])
  extends PairEvaluator[PixelImage[RGBA]] {
  override def logValue(reference: PixelImage[RGBA], sample: PixelImage[RGBA]): Double = {
    require(sample.domain == reference.domain, "IndependentPixelEvaluator: images must be comparable! (different sizes)")

    // ugly while for better performance, was a nice zip/map/case before :(
    var sum: Double = 0.0
    var x: Int = 0
    var transparencySum = 0.0
    while (x < reference.width) {
      var y: Int = 0
      while (y < reference.height) {
        val refCol: RGB = reference(x, y).toRGB
        val smp: RGBA = sample(x, y)
        val fg: Double = pixelEvaluator.logValue(refCol, smp.toRGB)
        val bg: Double = bgEvaluator.logValue(refCol)
        sum += smp.a * (fg - bg)
        transparencySum += smp.a
        y += 1
      }
      x += 1
    }
    if(transparencySum > 0) //was something rendered on the image?
      sum
    else Double.NegativeInfinity  // nothing was rendered on the image!
  }

  override def toString: String = {
    val builder = new StringBuilder(128)
    builder ++= "IndependentPixelEvaluator("
    builder ++= pixelEvaluator.toString
    builder ++= "/"
    builder ++= bgEvaluator.toString
    builder ++= ")"
    builder.mkString
  }
}

object IndependentPixelEvaluator {
  def apply(pixelEvaluator: PairEvaluator[RGB], bgEvaluator: DistributionEvaluator[RGB]) = new IndependentPixelEvaluator(pixelEvaluator, bgEvaluator)

  /**
    * construct an independent pixel likelihood of the target image under the image model */
  def apply(targetImage: PixelImage[RGBA],
            fgLikelihood: PairEvaluator[RGB],
            bgLikelihood: DistributionEvaluator[RGB]): DistributionEvaluator[PixelImage[RGBA]] = {
    IndependentPixelEvaluator(fgLikelihood, bgLikelihood).toDistributionEvaluator(targetImage)
  }

  /**
    * construct a likelihood with independent, isotropic Gaussian likelihoods at each pixel (very common case)
    * */
  def isotropicGaussian(targetImage: PixelImage[RGBA],
                        sdev: Double,
                        bgLikelihood: DistributionEvaluator[RGB]): DistributionEvaluator[PixelImage[RGBA]] = {
    val pixelEvaluator = IsotropicGaussianPixelEvaluator(sdev)
    IndependentPixelEvaluator(targetImage, pixelEvaluator, bgLikelihood)
  }

  /**
    * construct a likelihood with independent, isotropic Gaussian likelihoods at each pixel with a constant background model
    *
    * @param bgSdev background model becomes better for a pixel when its deviation to the rendered image color is larger than this value, measured in standard deviations sdev
    *
    */
  def isotropicGaussianConstantBackground(targetImage: PixelImage[RGBA],
                                          sdev: Double,
                                          bgSdev: Double): DistributionEvaluator[PixelImage[RGBA]] = {
    // standardized foreground evaluator, z transform to a std normal
    val pixelEvaluator: PairEvaluator[RGB] = new PairEvaluator[RGB] {
      override def logValue(first: RGB, second: RGB): Double = {
        val diff = (first - second).norm/sdev
        GaussianEvaluator.logDensity(diff, 0.0, 1.0)
      }
    }
    // background likelihood value at required distance in standard deviations
    val bgValue = GaussianEvaluator.logDensity(bgSdev, 0.0, 1.0)
    val bgEval = PixelEvaluators.ConstantPixelEvaluator[RGB](bgValue)
    IndependentPixelEvaluator(targetImage, pixelEvaluator, bgEval)
  }
}
