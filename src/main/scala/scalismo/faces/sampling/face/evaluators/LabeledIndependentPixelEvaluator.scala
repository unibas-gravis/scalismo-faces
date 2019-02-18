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
import scalismo.color.{RGB, RGBA}
import scalismo.faces.image.{LabeledPixelImage, PixelImage}
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.PairEvaluator

/**
  * evaluate each pixel independently, uses alpha channel to determine between foreground,
  *  background and label to destinguish between face and occlusion/non-face.
  *
  *  The face region is labeled as 1 (according to the following publication)
  *
  *  This evaluator is an implementation of Equation 2, 4, 5 and 7 of:
  *  Occlusion-aware 3D Morphable Face Models,
  *  Bernhard Egger, Andreas Schneider, Clemens Blumer, Andreas Morel-Forster, Sandro Schönborn, Thomas Vetter
  *  IN: British Machine Vision Conference (BMVC), September 2016
  *  https://dx.doi.org/10.5244/C.30.64
  *
  * */
class LabeledIndependentPixelEvaluator(val reference: PixelImage[RGBA], val pixelEvaluator: PairEvaluator[RGB], val bgEvaluator: DistributionEvaluator[RGB])
  extends DistributionEvaluator[LabeledPixelImage[RGBA]] {
  override def logValue(sample: LabeledPixelImage[RGBA]): Double = {
    require(sample.label.domain == reference.domain, "LabeledIndependentPixelEvaluator: images must be comparable! (different sizes)")

    // ugly while for better performance, was a nice zip/map/case before :(
    var sum: Double = 0.0
    var x: Int = 0

    // Equation 2
    while(x < reference.width) {
      var y: Int = 0
      while (y < reference.height) {
        val refCol: RGB = reference(x, y).toRGB
        val bg: Double = bgEvaluator.logValue(refCol)
        val smp: RGBA = sample.image(x, y)

        if (sample.label(x,y) == 1) {
          // this pixel is labeled as face
          if (smp.a >  1e-4) {
            // the pixel is coped by the face model: Equation 4
            val fg: Double = pixelEvaluator.logValue(refCol, smp.toRGB)
            sum += fg
          }
          else
            sum += bg // pixel is not coped by the face model
        }
        else {
          // this pixel is labeled as nonface: Equation 5 and 7
          if (smp.a > 1e-4) {
            // the pixel is coped by the face model
            sum += max(bg, pixelEvaluator.logValue(refCol, smp.toRGB))
          }
          else
            sum += bg // the pixel is not coped by the face model
        }
        y+=1
      }
      x+=1
    }
    sum
  }
  override def toString = {
    val builder = new StringBuilder(128)
    builder ++= "LabeledIndependentPixelEvaluator("
    builder ++= pixelEvaluator.toString
    builder ++= "/"
    builder ++= bgEvaluator.toString
    builder ++= ")"
    builder.mkString
  }
}

object LabeledIndependentPixelEvaluator {
  def apply(reference: PixelImage[RGBA], pixelEvaluator: PairEvaluator[RGB],  bgEvaluator: DistributionEvaluator[RGB]) = new LabeledIndependentPixelEvaluator(reference, pixelEvaluator, bgEvaluator)
}
