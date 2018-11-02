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

package scalismo.faces.color

import scalismo.color.{ColorSpaceOperations, RGB, RGBA}
import scalismo.faces.FacesTestSuite

class ColorSpaceOperationsTest extends FacesTestSuite {

  describe("Implicit ColorSpaceOperation operators are mapped to proper ColorSpaceOperations methods") {
    import scalismo.color.ColorSpaceOperations.implicits._

    case class V(v: Double)

    implicit object TestSpace extends ColorSpaceOperations[V] {
      override def add(pix1: V, pix2: V): V = V(pix1.v + pix2.v)
      override def multiply(pix1: V, pix2: V): V = V(pix1.v * pix2.v)
      override def dot(pix1: V, pix2: V): Double = pix1.v * pix2.v
      override def dimensionality: Int = 1
      override def scale(pix: V, l: Double): V = V(pix.v * l)
      override def zero: V = V(0.0)
    }

    val v1 = randomDouble
    val v2 = randomDouble
    val i1 = V(v1)
    val i2 = V(v2)
    val f = randomDouble

    it("+") { i1 + i2 shouldBe V(v1 + v2) }
    it("-") { i1 - i2 shouldBe V(v1 - v2) }
    it("x") { i1 x i2 shouldBe V(v1 * v2) }
    it("*") { i1 * f shouldBe V(v1 * f) }
    it("*:") { f *: i1 shouldBe V(v1 * f) }
    it("/") { (i1 / f).v shouldBe v1 / f +- 1e-6 }
    it("dot") { i1 dot i2 shouldBe v1 * v2 }
    it("normSq") { i1.normSq shouldBe v1 * v1 }
    it("unary-") { -i1 shouldBe V(-v1) }
  }

  describe("RGB has proper ColorSpaceOperations") {
    val i1 = randomRGB
    val i2 = randomRGB
    val f = randomDouble

    val ops = RGB.RGBOperations

    it("supports add") {
      i1 + i2 shouldBe ops.add(i1, i2)
    }
    it("supports multiply") {
      i1 x i2 shouldBe ops.multiply(i1, i2)
    }
    it("supports scale") {
      i1 * f shouldBe ops.scale(i1, f)
      f *: i1 shouldBe ops.scale(i1, f)
      i1 / f shouldBe ops.scale(i1, 1.0 / f)
    }
    it("supports normSq") {
      i1.dot(i1) shouldBe ops.normSq(i1)
    }
    it("supports dot") {
      i1 dot i2 shouldBe ops.dot(i1, i2)
    }
    it("has zero as black") {
      RGB.Black shouldBe ops.zero
    }
  }

  describe("RGBA has proper ColorSpaceOperations (involves A as 4th dimension)") {
    val i1 = randomRGBA
    val i2 = randomRGBA
    val f = randomDouble

    val ops = RGBA.RGBAOperations

    it("supports add") {
      i1 + i2 shouldBe ops.add(i1, i2)
    }
    it("supports multiply") {
      i1 x i2 shouldBe ops.multiply(i1, i2)
    }
    it("supports scale") {
      i1 * f shouldBe ops.scale(i1, f)
      f *: i1 shouldBe ops.scale(i1, f)
      i1 / f shouldBe ops.scale(i1, 1.0 / f)
    }
    it("supports normSq") {
      i1.dot(i1) shouldBe ops.normSq(i1)
    }
    it("supports dot") {
      i1 dot i2 shouldBe ops.dot(i1, i2)
    }
    it("has zero as black") {
      RGBA.BlackTransparent shouldBe ops.zero
    }
  }
}
