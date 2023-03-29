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
import scalismo.faces.image.{ImageBuffer, PixelImage, PixelImageDomain}
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.PairEvaluator

import scala.collection.mutable.ArrayBuffer

/**
 * Ignore 1-alpha pixels with small likelihood. Evaluate only on alpha-fraction with the largest likelihood.
 *
 * visualizationCallback: gives you the log likelihood values per pixel. Could be used to visualze the values per pixel.
 */
class TrimmedIndependentPixelEvaluator(val pixelEvaluator: PairEvaluator[RGB],
                                       val bgEvaluator: DistributionEvaluator[RGB],
                                       val alpha: Double,
                                       visualizationCallback: Option[PixelImage[Option[Double]] => Unit]
) extends PairEvaluator[PixelImage[RGBA]] {

  private val alphaClamped = math.min(math.max(0, alpha), 1)

  /** probability/density value for a pair (log value). */
  def logValue(reference: PixelImage[RGBA], sample: PixelImage[RGBA]): Double = {
    require(sample.domain == reference.domain,
            "TrimmedIndependentPixelEvaluator: images must be comparable! (different sizes)"
    )

    /** Visualisation. */
    def visualize(values: IndexedSeq[(Double, Int, Int)],
                  domain: PixelImageDomain,
                  callBack: PixelImage[Option[Double]] => Unit
    ): Unit = {
      val buffer = ImageBuffer.makeConstantBuffer[Option[Double]](domain.width, domain.height, None)
      values.foreach { case (lh: Double, x: Int, y: Int) => buffer(x, y) = Some(lh) }
      callBack(buffer.toImage)
    }
    var transparencySum = 0.0
    var values = ArrayBuffer[(Double, Int, Int)]()
    var x: Int = 0
    while (x < reference.width) {
      var y: Int = 0
      while (y < reference.height) {
        val smp = sample(x, y)
        if (smp.a > 1e-4f) {
          val ref = reference(x, y).toRGB
          val fg: Double = pixelEvaluator.logValue(ref, smp.toRGB)
          val bg: Double = bgEvaluator.logValue(ref)
          val entry = (fg - bg, x, y)
          values += entry
        }
        transparencySum += smp.a
        y += 1
      }
      x += 1
    }
    val nCount = math.floor(values.length.toFloat * alphaClamped).toInt
    if (transparencySum > 0 && nCount > 0) {
      // was something rendered on the image?
      val data = values.toIndexedSeq.sortBy { case (d: Double, x: Int, y: Int) => d }
      var sumTrimmed: Double = 0.0
      for (i <- 0 until nCount) {
        sumTrimmed += data(data.size - 1 - i)._1
      }
      if (visualizationCallback.isDefined)
        visualize(data.slice(data.size - 1 - nCount, data.size - 1), reference.domain, visualizationCallback.get)
      sumTrimmed
    } else {
      // nothing was rendered on the image!
      Double.NegativeInfinity
    }
  }

  override def toString: String = {
    val builder = new StringBuilder(128)
    builder ++= "TrimmedIndependentPixelEvaluator("
    builder ++= pixelEvaluator.toString
    builder ++= "/"
    builder ++= bgEvaluator.toString
    builder ++= s"alpha=$alphaClamped"
    builder ++= ")"
    builder.mkString
  }
}

object TrimmedIndependentPixelEvaluator {
  def apply(pixelEvaluator: PairEvaluator[RGB], bgEvaluator: DistributionEvaluator[RGB], alpha: Double) =
    new TrimmedIndependentPixelEvaluator(pixelEvaluator, bgEvaluator, alpha, None)

  def apply(pixelEvaluator: PairEvaluator[RGB],
            bgEvaluator: DistributionEvaluator[RGB],
            alpha: Double,
            visualisationCallback: PixelImage[Option[Double]] => Unit
  ) = new TrimmedIndependentPixelEvaluator(pixelEvaluator, bgEvaluator, alpha, Some(visualisationCallback))

}
