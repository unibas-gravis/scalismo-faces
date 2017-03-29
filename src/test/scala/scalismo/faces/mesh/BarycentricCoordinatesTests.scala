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

import scalismo.faces.FacesTestSuite
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.BarycentricCoordinates

class BarycentricCoordinatesTests extends FacesTestSuite{
  describe("pointInTriangle3D") {
    it("should be in triangle") {
      def pass(a: Double, b: Double) = math.abs(a - b) < 1e-5

      def testPoint(v1: Point[_3D], v2: Point[_3D], v3: Point[_3D]) = {
        val bcc = BarycentricCoordinates.randomUniform
        val point = bcc.interpolateProperty(v3, v2, v1)
        val bccNew = BarycentricCoordinates.pointInTriangle3D(point, v1, v2, v3)
        pass(bcc.a, bccNew.a) && pass(bcc.b, bccNew.b) && pass(bcc.c, bccNew.c)
      }
      val v1 = Point(2,3,1)
      val v2 = Point(3,2,2)
      val v3 = Point(1,1,3)

      val v21 = Point(2.1f,3.5f,-1)
      val v22 = Point(3,2,200)
      val v23 = Point(-1,-1,-3)

      assert(testPoint(v1, v2, v3) && testPoint(v21, v22, v23))
    }
  }
}
