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

package scalismo.faces.parameters

import java.net.URI

import scalismo.faces.FacesTestSuite
import scalismo.faces.momo.MoMoCoefficients

class MoMoInstanceTest extends FacesTestSuite {

  describe("A MoMoInstance") {

    it("can be created empty") {
      val emptyInstance = MoMoInstance.empty
      assert(emptyInstance.shape.isEmpty)
      assert(emptyInstance.color.isEmpty)
      assert(emptyInstance.expression.isEmpty)
      assert(emptyInstance.modelURI.toString.isEmpty)
    }

    it("can deliver MoMoCoefficients with the same coefficient numbers") {
      val shapeCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val colorCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val expressionCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())

      val instance = MoMoInstance(shapeCoeffs, colorCoeffs, expressionCoeffs, new URI(""))
      val coeffs = instance.coefficients
      coeffs.shape.toArray should contain theSameElementsInOrderAs shapeCoeffs
      coeffs.color.toArray should contain theSameElementsInOrderAs colorCoeffs
      coeffs.expression.toArray should contain theSameElementsInOrderAs expressionCoeffs
    }

    it("can be constructed from MoMoCoefficients with the same coefficient numbers") {
      val shapeCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val colorCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val expressionCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())

      val coeffs = MoMoCoefficients(shapeCoeffs, colorCoeffs, expressionCoeffs)
      val momoInstance = MoMoInstance.fromCoefficients(coeffs, new URI("protocol://randomURI"))
      momoInstance.shape should contain theSameElementsInOrderAs shapeCoeffs
      momoInstance.color should contain theSameElementsInOrderAs colorCoeffs
      momoInstance.expression should contain theSameElementsInOrderAs expressionCoeffs
      momoInstance.modelURI shouldBe new URI("protocol://randomURI")
    }

    it("can be constructed with a specified size") {
      val momoInstance = MoMoInstance.zero(3, 7, 9, new URI("protocol://randomURI"))
      momoInstance.shape.length shouldBe 3
      momoInstance.color.length shouldBe 7
      momoInstance.expression.length shouldBe 9
      momoInstance.modelURI shouldBe new URI("protocol://randomURI")
    }

    it("can be resized to a smaller size, keeping its valid entries") {
      val shapeCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val colorCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val expressionCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())

      val instance = MoMoInstance(shapeCoeffs, colorCoeffs, expressionCoeffs, new URI(""))
      val smallerInstance = instance.withNumberOfCoefficients(3, 3, 3)
      smallerInstance.shape.length shouldBe 3
      smallerInstance.color.length shouldBe 3
      smallerInstance.expression.length shouldBe 3
      smallerInstance.shape should contain theSameElementsInOrderAs shapeCoeffs.take(3)
      smallerInstance.color should contain theSameElementsInOrderAs colorCoeffs.take(3)
      smallerInstance.expression should contain theSameElementsInOrderAs expressionCoeffs.take(3)
    }

    it("can be resized to a larger size, keeping its entries") {
      val shapeCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val colorCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())
      val expressionCoeffs = IndexedSeq.fill(5)(rnd.scalaRandom.nextGaussian())

      val instance = MoMoInstance(shapeCoeffs, colorCoeffs, expressionCoeffs, new URI(""))
      val largerInstance = instance.withNumberOfCoefficients(7, 7, 7)
      largerInstance.shape.length shouldBe 7
      largerInstance.color.length shouldBe 7
      largerInstance.expression.length shouldBe 7

      largerInstance.shape.take(5) should contain theSameElementsInOrderAs shapeCoeffs
      largerInstance.color.take(5) should contain theSameElementsInOrderAs colorCoeffs
      largerInstance.expression.take(5) should contain theSameElementsInOrderAs expressionCoeffs

      largerInstance.shape.slice(5, 7) should contain theSameElementsInOrderAs Seq(0.0, 0.0)
      largerInstance.color.slice(5, 7) should contain theSameElementsInOrderAs Seq(0.0, 0.0)
      largerInstance.expression.slice(5, 7) should contain theSameElementsInOrderAs Seq(0.0, 0.0)
    }

  }
}
