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

import scalismo.color.RGBA
import scalismo.faces.image.{LabeledPixelImage, PixelImage}
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricImageRenderer
import scalismo.sampling.DistributionEvaluator

/** evaluate the rendered image with given image evaluator */
class ImageRendererEvaluator(val renderer: ParametricImageRenderer[RGBA],
                             val imageEvaluator: DistributionEvaluator[PixelImage[RGBA]]
) extends DistributionEvaluator[RenderParameter] {

  override def logValue(rps: RenderParameter): Double = {
    val rendered = renderer.renderImage(rps)
    imageEvaluator.logValue(rendered)
  }

  override def toString: String = "ImageRendererEvaluator(" + imageEvaluator.toString + ")"
}

object ImageRendererEvaluator {
  def apply(renderer: ParametricImageRenderer[RGBA], imageEvaluator: DistributionEvaluator[PixelImage[RGBA]]) =
    new ImageRendererEvaluator(renderer, imageEvaluator)
}

/** evaluate the rendered image with given image evaluator and a segmentation label */
class LabeledImageRendererEvaluator(val renderer: ParametricImageRenderer[RGBA],
                                    val imageEvaluator: DistributionEvaluator[LabeledPixelImage[RGBA]]
) extends DistributionEvaluator[(RenderParameter, PixelImage[Int])] {

  def logValue(masked: (RenderParameter, PixelImage[Int])): Double = {
    val rendered = renderer.renderImage(masked._1)
    imageEvaluator.logValue(LabeledPixelImage(rendered, masked._2))
  }
  override def toString = "LabeledImageRendererEvaluator(" + imageEvaluator.toString + ")"
}

object LabeledImageRendererEvaluator {
  def apply(renderer: ParametricImageRenderer[RGBA], imageEvaluator: DistributionEvaluator[LabeledPixelImage[RGBA]]) =
    new LabeledImageRendererEvaluator(renderer, imageEvaluator)
}

/** evaluate the transformed, rendered image with given image evaluator */
class MappedImageRendererEvaluator[A](val renderer: ParametricImageRenderer[RGBA],
                                      val evaluator: DistributionEvaluator[A],
                                      f: RenderParameter => PixelImage[RGBA] => A
) extends DistributionEvaluator[RenderParameter] {

  override def logValue(rps: RenderParameter): Double = {
    val rendered = f(rps)(renderer.renderImage(rps))
    evaluator.logValue(rendered)
  }

  override def toString: String = "ImageMappedRendererEvaluator(" + evaluator.toString + ")"
}

object ImageMappedRendererEvaluator {
  def apply[A](renderer: ParametricImageRenderer[RGBA],
               evaluator: DistributionEvaluator[A],
               f: RenderParameter => PixelImage[RGBA] => A
  ) = new MappedImageRendererEvaluator[A](renderer, evaluator, f)
}
