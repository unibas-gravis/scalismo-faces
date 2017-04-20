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

import scalismo.faces.FacesTestSuite
import scalismo.faces.image.PixelImage
import scalismo.faces.landmarks.{LandmarkDetectionMap, TLMSLandmark2D}
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry._

class LandmarkMapEvaluatorTests extends FacesTestSuite {

  private val rawDetectionMap = PixelImage(20, 20, (x, y) => {
    -0.5 * (Point(x, y) - Point(5, 5)).norm2
  })

  private val detectionMap = LandmarkDetectionMap("testLM", rawDetectionMap)

  private val initParam = RenderParameter.default
  private def lmRenderer(point: Point[_2D]) = new ParametricLandmarksRenderer {
    /** list of all known landmarks */
    override def allLandmarkIds: IndexedSeq[String] = IndexedSeq("testLM")

    /** check if landmark id is known */
    override def hasLandmarkId(lmId: String): Boolean = allLandmarkIds.contains(lmId)

    /** render landmark given by lmId */
    override def renderLandmark(lmId: String, parameter: RenderParameter): Option[TLMSLandmark2D] = {
      Some(TLMSLandmark2D("testLM", point, visible = true))
    }
  }

  describe("A LandmarksDetectionMap") {
    it("provides direct lookup") {
      detectionMap.logValues(5, 5) shouldBe 0.0
    }

    it("can be corrected with error rates of detection") {
      val fp = 0.05
      val fn = 0.07
      val corrected = detectionMap.correctCertaintyForErrorRates(fp, fn)
      math.exp(corrected.logValues(5, 5)) shouldBe 1.0 - fp
      math.exp(corrected.logValues(19, 19)) shouldBe 0.0 + fn
    }
  }

  describe("A LandmarksMapEvaluator") {
    val fp = 0.05
    val fn = 0.07
    val corrected = detectionMap.correctCertaintyForErrorRates(fp, fn)

    val randomPoints = Seq.fill(5){Point(rnd.scalaRandom.nextInt(rawDetectionMap.width), rnd.scalaRandom.nextInt(rawDetectionMap.height))}

    it("provides a constructor for isotropic Gaussian noise models with proper value on strongest detection") {
      val lmNoise = IsotropicGaussianPointEvaluator[_2D](5.0).toDistributionEvaluator(Point2D.origin)
      val lmEval = LandmarkMapEvaluator.withIsotropicGaussianNoise(Seq(corrected), lmRenderer(Point(5, 5)), 5.0)
      lmEval.logValue(initParam) shouldBe (corrected.logValues(5, 5) + lmNoise.logValue(Point2D.origin))
    }

    it("with precalculated noise model returns the proper value for a few points") {
      val lmNoise = IsotropicGaussianPointEvaluator[_2D](2.0)

      def checkPoint(point: Point[_2D]): Unit = {
        val renderer = lmRenderer(point)
        val lmEval = LandmarkMapEvaluator.withIsotropicGaussianNoise(Seq(corrected), renderer, 2.0)
        val explicitValue = { // brute force maximum finder
          corrected.logValues.mapWithIndex{ (v, x, y) =>
            v + lmNoise.logValue(Point(x, y), point)
          }.values.max
        }
        lmEval.logValue(initParam) shouldBe explicitValue +- 1e-10
      }

      // check for 5 random points
      for (pt <- randomPoints)
        checkPoint(pt)
    }

    it("with an explicit noise model returns the proper value for a few points") {
      val lmNoise = IsotropicGaussianPointEvaluator[_2D](2.0)

      def checkPoint(point: Point[_2D]): Unit = {
        val renderer = lmRenderer(point)
        val lmEvalExplicit = LandmarkMapEvaluator.withLandmarksLikelihood(Seq(corrected), renderer, lmNoise)
        val explicitValue = { // brute force maximum finder
          corrected.logValues.mapWithIndex{ (v, x, y) =>
            v + lmNoise.logValue(Point(x, y), point)
          }.values.max
        }
        lmEvalExplicit.logValue(initParam) shouldBe explicitValue +- 1e-10
      }

      // check for 5 random points
      for (pt <- randomPoints)
        checkPoint(pt)
    }
  }
}
