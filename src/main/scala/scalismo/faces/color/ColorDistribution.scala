/*
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

package scalismo.faces.color
import scalismo.color.RGB
import scalismo.utils.Random

import scala.collection.immutable.IndexedSeq

trait ColorDistribution {

  def sample(implicit rnd: Random): RGB

  def normalizer: Double

  def logNormalizer: Double

  def evaluate(color: RGB): Double

  def evaluateLog(color: RGB): Double
}

case class GaussianColorDistribution(mean: RGB, sdev: RGB) extends ColorDistribution {
  override def sample(implicit rnd: Random): RGB = {
    val r = RGB(rnd.scalaRandom.nextGaussian(), rnd.scalaRandom.nextGaussian(), rnd.scalaRandom.nextGaussian())
    ((r x sdev) + mean)
  }

  override val normalizer = 1.0 / math.pow(2 * math.Pi, 1.5) / math.sqrt(sdev.toVector.norm2)

  override val logNormalizer = -1.5 * math.log(2 * math.Pi) - 0.5 * math.log(sdev.toVector.norm2)

  override def evaluate(color: RGB): Double = math.exp(evaluateLog(color)) * normalizer

  override def evaluateLog(color: RGB): Double = {
    val d = (color - mean) / sdev
    -0.5 * d.toVector.norm2 + logNormalizer
  }
}

object GaussianColorDistribution {
  def apply(colors: IndexedSeq[RGB]): GaussianColorDistribution = {
    val n = colors.length
    val sum = colors.reduce { _ + _ }
    val sqSum = colors.reduce((sum, a) => sum + a.map { x => x * x })
    val mean = sum / n
    val variance = sqSum / n - (mean x mean)
    val sdev = variance.map(math.sqrt)
    GaussianColorDistribution(mean, sdev)
  }
}

case object UniformColorDistribution extends ColorDistribution {
  override def sample(implicit rnd: Random): RGB =
    RGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  override def normalizer: Double = 1.0

  override def logNormalizer: Double = 0.0

  override def evaluate(color: RGB): Double = 1.0

  override def evaluateLog(color: RGB): Double = 0.0
}
