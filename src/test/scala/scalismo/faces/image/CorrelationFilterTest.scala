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

package scalismo.faces.image

import scalismo.faces.FacesTestSuite
import scalismo.faces.image.filter.{Gradient, SeparableCorrelationFilter}
import org.scalatest.PrivateMethodTester

import scala.collection.IndexedSeq

class CorrelationFilterTest extends FacesTestSuite with PrivateMethodTester {

  describe("A PixelImage filtered with kernels") {

    it("is consistent when filtered with SobelX") {
      val testImg = PixelImage(ColumnMajorImageDomain(3, 3), Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)).withAccessMode(AccessMode.Repeat())
      val Sx = Gradient.sobelX[Double]
      val filtered: PixelImage[Double] = Sx.filter(testImg.withAccessMode(AccessMode.Repeat()))
      filtered.values.toIndexedSeq should be(IndexedSeq(12.0, 12.0, 12.0, 24.0, 24.0, 24.0, 12.0, 12.0, 12.0))
    }

    it("is consistent when filtered with SobelY") {
      val testImg = PixelImage(ColumnMajorImageDomain(3, 3), Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)).withAccessMode(AccessMode.Repeat())
      val Sx = Gradient.sobelY[Double]
      val filtered: PixelImage[Double] = Sx.filter(testImg.withAccessMode(AccessMode.Repeat()))
      filtered.transposed.values.toIndexedSeq should be(IndexedSeq(4.0, 4.0, 4.0, 8.0, 8.0, 8.0, 4.0, 4.0, 4.0))
    }

    it("yields a symmetric image (I==I.t) for a separable kernel (regression test for access modes in separable filters)") {
      val checkers = PixelImage(15, 15, (x,y) => if((x+y)%2==0) 1.0 else 0.0 )
      val blurKernel = PixelImage(PixelImageDomain(3, 1), IndexedSeq(0.25, 0.5, 0.25).toIndexedSeq)
      val blurred = checkers.filter(SeparableCorrelationFilter(blurKernel, blurKernel.transposed))
      val diff = blurred.transposed.zip(blurred).map{case(b,c) => math.pow(b - c, 2)}
      diff.values.sum should be < 1e-10
    }

  }
}
