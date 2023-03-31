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

import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{_2D, Point}
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.{PairEvaluator, ProductEvaluator}

/**
 * likelihood evaluator for a single landmark position
 */
class LandmarkPointEvaluator(targetLandmark: TLMSLandmark2D,
                             pointEvaluator: PairEvaluator[Point[_2D]],
                             landmarksRenderer: ParametricLandmarksRenderer
) extends DistributionEvaluator[RenderParameter] {

  override def logValue(sample: RenderParameter): Double = {
    val lmLocation: TLMSLandmark2D = landmarksRenderer.renderLandmark(targetLandmark.id, sample).get
    pointEvaluator.logValue(lmLocation.point, targetLandmark.point)
  }
}

object LandmarkPointEvaluator {

  /** construct a single landmark evaluator with the provided noise model */
  def apply(targetLandmark: TLMSLandmark2D,
            pointEvaluator: PairEvaluator[Point[_2D]],
            landmarksRenderer: ParametricLandmarksRenderer
  ): LandmarkPointEvaluator = new LandmarkPointEvaluator(targetLandmark, pointEvaluator, landmarksRenderer)

  /** construct many independent landmark evaluators with the same point noise model */
  def apply(targetLandmarks: Seq[TLMSLandmark2D],
            pointEvaluator: PairEvaluator[Point[_2D]],
            landmarksRenderer: ParametricLandmarksRenderer
  ): DistributionEvaluator[RenderParameter] = {
    val allLMEvals = targetLandmarks.map { lm => LandmarkPointEvaluator(lm, pointEvaluator, landmarksRenderer) }
    ProductEvaluator(allLMEvals: _*)
  }

  /**
   * construction of an isotropic Gaussian likelihood for a single landmark encapsulates the general PairEvaluator for
   * this explicit, very common case
   *
   * @param targetLandmark
   *   the observed target landmark, calculates likelihood of this position under rendered model
   * @param sdev
   *   standard deviation of isotropic Gaussian, typically in pixels (TLMS refers to image locations)
   * @param landmarksRenderer
   *   renderer to generate instance
   */
  def isotropicGaussian(targetLandmark: TLMSLandmark2D,
                        sdev: Double,
                        landmarksRenderer: ParametricLandmarksRenderer
  ): LandmarkPointEvaluator = {
    val pointEval = IsotropicGaussianPointEvaluator[_2D](sdev)
    new LandmarkPointEvaluator(targetLandmark, pointEval, landmarksRenderer)
  }

  /**
   * construction of an isotropic Gaussian likelihood for a set of landmarks encapsulates the general PairEvaluator for
   * this explicit, very common case
   *
   * @param targetLandmarks
   *   all observed target landmarks, calculates likelihood of these positions under rendered model (product,
   *   independence assumption)
   * @param sdev
   *   standard deviation of isotropic Gaussian, typically in pixels (TLMS refers to image locations)
   * @param landmarksRenderer
   *   renderer to generate instance
   */
  def isotropicGaussian(targetLandmarks: Seq[TLMSLandmark2D],
                        sdev: Double,
                        landmarksRenderer: ParametricLandmarksRenderer
  ): DistributionEvaluator[RenderParameter] = {
    val pointEval = IsotropicGaussianPointEvaluator[_2D](sdev)
    LandmarkPointEvaluator(targetLandmarks, pointEval, landmarksRenderer)
  }
}
