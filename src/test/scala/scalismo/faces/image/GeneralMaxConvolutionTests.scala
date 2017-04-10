/*
 * Copyright 2017 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.image

import breeze.numerics.log
import org.scalactic.Equality
import scalismo.faces.FacesTestSuite
import scalismo.faces.image.filter.GeneralMaxConvolution
import scalismo.geometry._
import scalismo.sampling.DistributionEvaluator
import scala.language.reflectiveCalls

class GeneralMaxConvolutionTests extends FacesTestSuite {

  new Equality[Double] {
    def areEqual(a: Double, b: Any): Boolean =
      b match {
        case p: Double => a === p +- 0.0
        case _ => false
      }
  }

  class Evaluator1D(sdev: Double) extends DistributionEvaluator[Point[_1D]] {
    override def logValue(sample: Point[_1D]): Double = -math.pow(sample.x, 2)/2.0/sdev/sdev
  }

  class Evaluator2D(sdev: Double) extends DistributionEvaluator[Point[_2D]] {
    override def logValue(sample: Point[_2D]): Double = -sample.toVector.norm2/2.0/sdev/sdev
  }

  describe("general max convolution") {

    def fixture = new {
      val noise = 1
      val eval = new Evaluator1D(noise)
      val eval2d = new Evaluator2D(noise)
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
        val result = GeneralMaxConvolution.maxConvolution1D(f.row.toArray, f.eval)

        def evalForPosition(p: Int) {
          val allValues = f.row.toArray.zipWithIndex.map { case(v, index) => v + f.eval.logValue(Point1D(index - p)) }
          result(p) shouldEqual allValues.max
        }

        for (p <- 0 until f.width) {
          evalForPosition(p)
        }

      }
    }

    describe("in 2d") {
      it("should be correct using the separable version") {
        val f = fixture
        val result = GeneralMaxConvolution.separable2D(f.image, f.eval)

        def evalForPosition(p: Int, q: Int) {
          val allValues = (0 until f.width).flatMap { x =>
            (0 until f.height).map { y =>
              f.image(x, y) + f.eval2d.logValue(Point2D(x - p, y - q))
            }
          }
          result(p, q) shouldEqual allValues.max
        }

        for (p <- 0 until f.width; q <- 0 until f.height) {
          evalForPosition(p, q)
        }
      }
    }
  }
}

