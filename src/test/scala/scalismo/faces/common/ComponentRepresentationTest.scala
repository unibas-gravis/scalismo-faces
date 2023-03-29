/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.common

import scalismo.faces.FacesTestSuite
import scalismo.color.{RGB, RGBA}
import scalismo.common.ComponentRepresentation
import scalismo.geometry.{_2D, _3D, EuclideanVector}

class ComponentRepresentationTest extends FacesTestSuite {

  def testComponentRepresentation[A](
    expectedDimensionality: Int
  )(implicit vectorizer: ComponentRepresentation[A]): Unit = {
    // dim
    val d = vectorizer.size
    it("should have the right dimensionality") {
      d shouldBe expectedDimensionality
    }

    val data = Array.fill(d)(randomDouble)
    val value = vectorizer.fromArray(data)

    // toArray and fromArray
    it("toArray and fromArray compose to identity") {
      vectorizer.toArray(vectorizer.fromArray(data)).sameElements(data) shouldBe true
    }

    // intoArray and toArray
    it("intoArray and fromArray are consistent") {
      val test = new Array[Double](d)
      vectorizer.intoArray(value, test)
      test.sameElements(data) should be(true)
    }

    // component and toArray
    it("component(i) and fromArray are consistent") {
      for (i <- 0 until d)
        vectorizer.component(value, i) shouldBe data(i)
    }

    // fromComponents and fromArray
    it("fromComponents and fromArray are consistent") {
      def comp(i: Int): Double = data(i)
      vectorizer.fromComponents(comp) shouldBe value
    }
  }

  describe("A ComponentRepresentation[EuclideanVector[_2D]]") {
    testComponentRepresentation[EuclideanVector[_2D]](2)
  }

  describe("A ComponentRepresentation[EuclideanVector[_3D]]") {
    testComponentRepresentation[EuclideanVector[_3D]](3)
  }

  describe("A ComponentRepresentation[RGB]") {
    testComponentRepresentation[RGB](3)
  }

  describe("A ComponentRepresentation[RGBA]") {
    testComponentRepresentation[RGBA](4)
  }
}
