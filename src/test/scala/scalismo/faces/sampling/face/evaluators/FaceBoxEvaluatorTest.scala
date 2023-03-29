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
import scalismo.geometry.{EuclideanVector, Point, Point2D}

class FaceBoxEvaluatorTest extends FacesTestSuite {

  val yamlCodeBox =
    """
      |%YAML:1.0
      |FaceBox: [ 66, 111, 355, 355 ]
      |pFace: 9.6226720524414477e-01
    """.stripMargin

  describe("A Face box (face detection candidate)") {

    var fb: FaceBox = FaceBox(Point2D.origin, Point2D.origin, 0.0)

    it("is read properly") {
      fb = FaceBox.fromYAML(yamlCodeBox).get
      fb.topLeft shouldBe Point(66, 111)
      fb.size shouldBe EuclideanVector(355, 355)
      fb.certainty shouldBe 0.9623 +- 1e-4
    }

    it("has the position (center)") {
      fb.center shouldBe Point(243.5, 288.5)
      fb.bottomRight shouldBe Point(421, 466)
    }

    it("has the proper scale") {
      fb.scale shouldBe 167.348 +- 1e-2
    }
  }
}
