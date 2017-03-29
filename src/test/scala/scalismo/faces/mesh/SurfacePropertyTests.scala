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

package scalismo.faces.mesh

import scalismo.common.PointId
import scalismo.faces.FacesTestSuite
import scalismo.geometry.IntVector
import scalismo.mesh.{SurfacePointProperty, TriangleCell, TriangleList, TriangleProperty}

class SurfacePropertyTests extends FacesTestSuite {

  describe("MeshSurfaceProperty equalities:") {

    // different triangulation instances with identical data
    val t1 = TriangleList(IndexedSeq(TriangleCell(PointId(0), PointId(1), PointId(2)), TriangleCell(PointId(1), PointId(2), PointId(3))))
    val t2 = TriangleList(IndexedSeq(TriangleCell(PointId(0), PointId(1), PointId(2)), TriangleCell(PointId(1), PointId(2), PointId(3))))

    // different data arrays with same data
    val d1: IndexedSeq[Double] = (0 to 5).map(_.toDouble)
    val d2: IndexedSeq[Double] = (0 to 5).map(_.toDouble)
    val d3: IndexedSeq[Double] = (5 to 10).map(_.toDouble)

    describe("SurfacePointProperty:") {
      val sp = SurfacePointProperty(t1, d1)
      val spSame = SurfacePointProperty(t2, d2)
      val spDiff = SurfacePointProperty(t2, d3)

      it("different instances with same data values are equal") {
        sp should be(spSame)
      }

      it("different instances with different data are not equal") {
        sp should not be spDiff
      }
    }

    describe("TriangleProperty:") {
      val tp = TriangleProperty(t1, d1)
      val tpSame = TriangleProperty(t2, d2)
      val tpDiff = TriangleProperty(t2, d3)

      it("different instances with same data values are equal") {
        tp should be(tpSame)
      }

      it("different instances with different data are not equal") {
        tp should not be tpDiff
      }
    }

    describe("VertexPropertyPerTriangle:") {
      val i1 = IndexedSeq(IntVector(0, 1, 2), IntVector(1, 2, 3))
      val i2 = IndexedSeq(IntVector(0, 1, 2), IntVector(1, 2, 3))
      val iDiff = IndexedSeq(IntVector(1, 2, 0), IntVector(3, 2, 1))

      val vp = VertexPropertyPerTriangle(t1, i1, d1)
      val vpSame = VertexPropertyPerTriangle(t2, i2, d2)
      val vpDiffData = VertexPropertyPerTriangle(t1, i2, d3)
      val vpDiffIndex = VertexPropertyPerTriangle(t1, iDiff, d2)

      it("different instances with same data values are equal") {
        vp should be(vpSame)
      }

      it("different instances with different data are not equal") {
        vp should not be vpDiffData
      }

      it("different instances with different indices are not equal") {
        vp should not be vpDiffIndex
      }
    }
  }
}
