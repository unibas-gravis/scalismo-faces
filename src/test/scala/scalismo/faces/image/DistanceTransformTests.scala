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

import scalismo.faces.FacesTestSuite
import scalismo.faces.image.filter.DistanceTransform
import scalismo.geometry.Vector2D

class DistanceTransformTests extends FacesTestSuite {

  /** slow and explicit test implementation of distance transform: explicit calculation everywhere */
  def stupidDistanceTransform(image: PixelImage[Boolean]): PixelImage[Double] = {
    PixelImage(image.width, image.height, (x, y) => {
      val distToEachImagePoint = PixelImage.view(image.width, image.height, (x1, y1) => Vector2D(x - x1, y - y1).norm)
      val maskedDistances = distToEachImagePoint.zip(image).mapLazy{case(dist, obj) => if (obj) dist else Double.PositiveInfinity}
      val minDistance = maskedDistances.values.min
      minDistance
    })
  }

  describe("In DistanceTransform") {
    val simpleImage = PixelImage(50, 50, (x, y) => Vector2D(x - 25, y - 15).norm <= 5)
    val twoObjects = PixelImage(50, 50, (x, y) => Vector2D(x - 25, y - 15).norm <= 5 || Vector2D(x - 45, y - 45).norm <= 10)

    it("our reference distance transform is correct for the simple image") {
      val distanceImage = stupidDistanceTransform(simpleImage)
      distanceImage(12, 15) shouldBe 8.0 +- 1e-5
      distanceImage(12, 35) shouldBe math.sqrt(13*13 + 20*20) - 5 +- 0.5
      distanceImage(25, 35) shouldBe 15.0 +- 1e-5
      distanceImage(32, 35) shouldBe math.sqrt(7*7 + 20*20) - 5 +- 0.5
      distanceImage(32, 15) shouldBe 2.0 +- 1e-5
    }

    it("our reference distance transform is correct for two objects") {
      val distanceImage = stupidDistanceTransform(twoObjects)
      distanceImage(12, 15) shouldBe 8.0 +- 1e-5
      distanceImage(12, 35) shouldBe math.sqrt(13*13 + 20*20) - 5 +- 1
      distanceImage(25, 35) shouldBe (Vector2D(25, 35) - Vector2D(45, 45)).norm - 10 +- 0.5
      distanceImage(32, 35) shouldBe (Vector2D(32, 35) - Vector2D(45, 45)).norm - 10 +- 0.5
      distanceImage(32, 15) shouldBe 2.0 +- 1e-5
    }

    describe("a euclidianDistanceTransform") {
      it("of a simple 2D image is the same as the slow, explicit distance transform") {
        val distanceImage = DistanceTransform.euclidian(simpleImage)
        val stupidDistanceImage = stupidDistanceTransform(simpleImage)
        distanceImage shouldBe stupidDistanceImage
      }

      it("of two objects is the same as the slow, explicit distance transform") {
        val distanceImage = DistanceTransform.euclidian(twoObjects)
        val stupidDistanceImage = stupidDistanceTransform(twoObjects)
        distanceImage shouldBe stupidDistanceImage
      }
    }

    describe("a signedDistanceTransform"){
      describe("of a simple image") {
        val signedDistance = DistanceTransform.signedEuclidian(simpleImage)

        it("is correct outside") {
          val distanceTransform = DistanceTransform.euclidian(simpleImage)
          assert(
            signedDistance.zip(distanceTransform).values.forall{
              case(signed, dist) => if (dist > 0.0) signed == dist else true}
          )
        }

        it("is correct inside") {
          val distanceTransform = DistanceTransform.euclidian(simpleImage.map{!_})
          assert(
            signedDistance.zip(distanceTransform).values.forall{
              case(signed, dist) => if (dist < 0.0) signed == -dist else true}
          )
        }
      }

      describe("of two objects") {
        val signedDistance = DistanceTransform.signedEuclidian(twoObjects)

        it("is correct outside") {
          val distanceTransform = DistanceTransform.euclidian(twoObjects)
          assert(
            signedDistance.zip(distanceTransform).values.forall{
              case(signed, dist) => if (dist > 0.0) signed == dist else true}
          )
        }

        it("is correct inside") {
          val distanceTransform = DistanceTransform.euclidian(twoObjects.map{!_})
          assert(
            signedDistance.zip(distanceTransform).values.forall{
              case(signed, dist) => if (dist < 0.0) signed == -dist else true}
          )
        }
      }
    }
  }
}
