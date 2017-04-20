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

import scalismo.faces.image.PixelImage
import scalismo.faces.image.filter.GeneralMaxConvolution
import scalismo.faces.landmarks.LandmarkDetectionMap
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{Point, Point1D, _1D, _2D}
import scalismo.sampling.DistributionEvaluator
import scalismo.sampling.evaluators.{PairEvaluator, ProductEvaluator}

/**
  * The LandmarkMapEvaluator evaluates landmark positions by a simple look-up in a LandmarkDetectionMap. These
  * maps usually include a noise-model through precomputation.
  *
  * @param detectionMap detection map of this landmark, contains log certainty of detecting landmark at every location
  * @param renderer landmarks renderer to calculate landmarks position of current sample
  */
case class LandmarkMapEvaluator(detectionMap: LandmarkDetectionMap,
                                renderer: ParametricLandmarksRenderer)
  extends DistributionEvaluator[RenderParameter] {

  override def logValue(sample: RenderParameter): Double = {
    val landmarkPosition = renderer.renderLandmark(detectionMap.tag, sample).get
    val point = landmarkPosition.point
    val x = point.x.toInt
    val y = point.y.toInt
    if (detectionMap.logValues.domain.isDefinedAt(x, y)) {
      detectionMap.logValues(x, y)
    } else {
      Double.NegativeInfinity
    }
  }

}

object LandmarkMapEvaluator {

  /**
    * Convenience constructor for combining multiple landmark map evaluators in a ProductEvaluator.
    */
  def apply(detectionMaps: Seq[LandmarkDetectionMap],
            renderer: ParametricLandmarksRenderer): ProductEvaluator[RenderParameter] = {
    val mapEvaluators = detectionMaps.map{map => LandmarkMapEvaluator(map, renderer)}
    ProductEvaluator(mapEvaluators: _*)
  }

  /**
    * Prepare landmarks detection maps with an isotropic Gaussian noise model, combination in a ProductEvaluator (independent landmarks)
    */
  def withIsotropicGaussianNoise(detectionMaps: Seq[LandmarkDetectionMap],
                                 renderer: ParametricLandmarksRenderer,
                                 stddevNoiseModel: Double): ProductEvaluator[RenderParameter] = {
    val mapEvaluators = detectionMaps.map { detectionMap =>
      // precalculate best combination of detection certainty and landmarks noise model with a maximum convolution
      val detectionMapWithNoise = GeneralMaxConvolution.separable2D(
        detectionMap.logValues,
        IsotropicGaussianPointEvaluator[_1D](stddevNoiseModel).toDistributionEvaluator(Point1D(0))
      )
      new LandmarkMapEvaluator(LandmarkDetectionMap(detectionMap.tag, detectionMapWithNoise), renderer)
    }
    ProductEvaluator(mapEvaluators: _*)
  }

  /**
    * Prepare landmarks detection maps with the specified noise model, combination in a ProductEvaluator (independent landmarks)
    * WARNING: uses a slow full maximum search internally, use only for exotic noise models
    * The resulting map evaluator is lazy - it might still work efficiently if you do not have too many evaluations
    */
  def withLandmarksLikelihood(detectionMaps: Seq[LandmarkDetectionMap],
                              renderer: ParametricLandmarksRenderer,
                              landmarksLikelihood: PairEvaluator[Point[_2D]]): ProductEvaluator[RenderParameter] = {
    val mapEvaluators = detectionMaps.map { detectionMap =>
      // cache all computations
      val detectionMapWithNoise = PixelImage.view(detectionMap.logValues.domain, { (x, y) =>
        // find best combination (max) of detection certainty and landmarks noise model
        // WARNING: very inefficient full search
        val currentPoint = Point(x, y)
        detectionMap.logValues.values.zipWithIndex.map{ case (value, i) =>
          val (tx, ty) = detectionMap.logValues.domain.coordsFromIndex(i)
          value + landmarksLikelihood.logValue(Point(tx, ty), currentPoint)
        }.max
      })
      // the evaluator is lazy, best combinations are only computed where it is evaluated
      new LandmarkMapEvaluator(LandmarkDetectionMap(detectionMap.tag, detectionMapWithNoise), renderer)
    }
    ProductEvaluator(mapEvaluators: _*)
  }
}