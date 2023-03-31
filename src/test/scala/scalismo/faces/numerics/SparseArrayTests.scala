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

package scalismo.faces.numerics

import scalismo.faces.FacesTestSuite

class SparseArrayTests extends FacesTestSuite {

  describe("A SparseArray") {
    val index = Array(0, 1, 5, 6)
    val data = Array(1.0, 2.0, 3.0, 4.0)

    val a = new SparseArray(index, data, index.length, 10)

    it("supports reading nnz values") {
      a(0) shouldBe 1.0
      a(1) shouldBe 2.0
      a(6) shouldBe 4.0
    }

    it("support reading of zero values") {
      a(2) shouldBe 0.0
      a(9) shouldBe 0.0
    }

    it("supports writing of existing values") {
      a(5) = -7.0
      a(5) shouldBe -7.0
      a(4) shouldBe 0.0
      a(6) shouldBe 4.0
    }

    it("supports writing at the end (w/o affecting its neighbours)") {
      a(7) = -1.0
      a(7) shouldBe -1.0
      a(6) shouldBe 4.0
      a(8) shouldBe 0.0
    }

    it("supports writing within") {
      a(2) = -2.0
      a(2) shouldBe -2.0
      a(1) shouldBe 2.0
      a(3) shouldBe 0.0
      a(5) shouldBe -7.0
    }

    it("keeps its index sorted") {
      var i = 1
      while (i < a.nnz) {
        a.index(i) shouldBe >(a.index(i - 1))
        i += 1
      }
    }

    it("can be transformed to a dense array") {
      val d = a.toDense
      d.toIndexedSeq shouldBe IndexedSeq(1.0, 2.0, -2.0, 0.0, 0.0, -7.0, 4.0, -1.0, 0.0, 0.0)
    }
  }

  describe("A SparseAccumulator") {
    // @todo
  }
}
