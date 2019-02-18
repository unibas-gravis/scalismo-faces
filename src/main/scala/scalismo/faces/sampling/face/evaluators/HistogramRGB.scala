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

import scalismo.color.{RGB, RGBA}
import scalismo.faces.image.PixelImage
import scalismo.sampling.DistributionEvaluator

/**
 * Provides specific implementation of a Histogram for a sequence/image of RGB values
 * for RGBA images alpha value is used as mask
 */
case class HistogramRGB private (counts: Array[Int], binsPerChannel: Int) extends DistributionEvaluator[RGB] {
  require(binsPerChannel * binsPerChannel * binsPerChannel == counts.length, "bin size does not match length of sequence")
  private val total: Int = counts.sum
  private val logNorm = 3 * Math.log(binsPerChannel)
  private val logTot = Math.log(total)
  val distributionPrecalc: Array[Double] = counts.map(c => Math.log(c) - logTot + logNorm)

  def frequency(color: RGB): Double = counts(HistogramRGB.RGBtoIndex(color, binsPerChannel)).toDouble / total
  def distribution(color: RGB): Double = distributionPrecalc(HistogramRGB.RGBtoIndex(color, binsPerChannel))
  def binSize: Double = 1.0 / binsPerChannel / binsPerChannel / binsPerChannel
  def hist2String: String = counts.toIndexedSeq.toString
  override def logValue(color: RGB): Double = distributionPrecalc(HistogramRGB.RGBtoIndex(color, binsPerChannel))
  override def toString: String = "HistogramRGBEvaluator(" + binsPerChannel + ")"

}

object HistogramRGB {
  def apply(image: Seq[RGB], binsPerChannel: Int, init: Int = 0 ) = new HistogramRGB(calculateCounts(image, binsPerChannel, init ), binsPerChannel)
  def fromImageRGB(image: PixelImage[RGB], binsPerChannel: Int, init: Int = 0) = HistogramRGB(image.values.toIndexedSeq, binsPerChannel, init)
  def fromImageRGBA(image: PixelImage[RGBA], binsPerChannel: Int, init: Int = 0) = HistogramRGB(image.values.filter(p => p.a > 1e-4).map(p => p.toRGB).toIndexedSeq, binsPerChannel, init)

  private def RGBtoIndex(color: RGB, binsPerChannel: Int): Int = {
    require(color.isInBounds, "color is invalid (not in bounds between 0 and 1)")
    val rIndex: Int = Math.min(Math.round(color.r * binsPerChannel - 0.5), binsPerChannel - 1).toInt
    val gIndex: Int = Math.min(Math.round(color.g * binsPerChannel - 0.5), binsPerChannel - 1).toInt
    val bIndex: Int = Math.min(Math.round(color.b * binsPerChannel - 0.5), binsPerChannel - 1).toInt
    rIndex * binsPerChannel * binsPerChannel + gIndex * binsPerChannel + bIndex
  }

  private def calculateCounts(image: Seq[RGB], binsPerChannel: Int, init:Int): Array[Int] = {
    val counter = Array.fill(binsPerChannel * binsPerChannel * binsPerChannel) { init }
    image.foreach { p => counter(RGBtoIndex(p, binsPerChannel)) += 1 }
    counter
  }

}