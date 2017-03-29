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

package scalismo.faces.utils

import breeze.numerics.log
import org.scalactic.Equality
import scalismo.faces.FacesTestSuite
import scalismo.faces.image.PixelImage
import scalismo.faces.sampling.face.evaluators.PointEvaluators.IsotropicGaussianPointEvaluator
import scalismo.geometry.{Point1D, Point2D, _1D, _2D}

class GeneralMaxConvolutionTest extends FacesTestSuite {

  new Equality[Double] {
    def areEqual(a: Double, b: Any): Boolean =
      b match {
        case p: Double => a === p +- 0.0
        case _ => false
      }
  }

  describe("general max convolution") {

    def fixture = new {
      val noise = 1
      val eval = IsotropicGaussianPointEvaluator[_1D](noise).toDistributionEvaluator(Point1D(0f))
      val eval2d = IsotropicGaussianPointEvaluator[_2D](noise).toDistributionEvaluator(Point2D(0f, 0f))
      val width = 5
      val height = 5
      val image = PixelImage(width, height, (x, y) => {
        (x + y).toDouble
      }).map(p => log(p / (width + height - 2) * 0.9 + 0.1))
      val row = image.row(0)
    }

    describe("in 1d") {
      it("should be calculated correctly") {
        val f = fixture
        val result = GeneralMaxConvolution.full1DMaxConvolution(f.row.toArray, f.eval)

        def evalForPosition(p: Int) {
          val values = f.row.toArray.zipWithIndex.map { e => e._1 + f.eval.logValue(Point1D(e._2 - p)) }
          result(p) shouldEqual values.max
        }

        for (p <- 0 until f.width) {
          evalForPosition(p)
        }

      }
    }

    describe("in 2d") {
      it("should be correct using the separable version") {
        val f = fixture
        val result = GeneralMaxConvolution.separableMaxConvolution(f.image, f.eval)

        def evalForPosition(p: Int, q: Int) {
          val values = (0 until f.width).flatMap { x =>
            (0 until f.height).map { y =>
              f.image(x, y) + f.eval2d.logValue(Point2D(x - p, y - q))
            }
          }
          result(p, q) shouldEqual values.max
        }

        for (p <- 0 until f.width; q <- 0 until f.height) {
          evalForPosition(p, q)
        }
      }
    }
  }
}

