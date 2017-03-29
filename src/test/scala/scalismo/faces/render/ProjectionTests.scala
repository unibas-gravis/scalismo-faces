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

package scalismo.faces.render

import breeze.linalg.DenseVector
import scalismo.faces.FacesTestSuite
import scalismo.geometry.{Point, Vector, _2D, _3D}

class ProjectionTests extends FacesTestSuite {

  val frustum: Frustum = Frustum.fromFocalWithSensor(50, Vector(35, 26), 0.5e3, 50e3)

  def p2d(p: Point[_3D]): Point[_2D] = Point(p.x, p.y)

  describe("A Frustum") {
    // simple frustum
    val fr = Frustum(-1, 1, -1, 1, 1, 3)
    val Frustum(l, r, b, t, n, f) = fr

    val x3d = Point(0.5, -0.5, -2)

    it("should lead to a known and valid pinhole projection") {
      val p = FrustumPinholeProjection(fr)
      val x3dp = p(x3d)
      x3dp shouldBe Point(0.25, -0.25, 0.5)
    }

    it("should lead to a known and valid orthographic projection") {
      val p = FrustumOrthographicProjection(fr)
      val x3dp = p(x3d)
      x3dp shouldBe Point(0.5, -0.5, 0.0)
    }

    it("can be scaled") {
      val sF = fr.scale(0.3, 1.5)
      sF shouldBe Frustum(l * 0.3, r * 0.3, b * 1.5, t * 1.5, n, f)
    }

    it("can change its near clipping plane without affecting the transformation") {
      val nF = fr.copy(far = 100).withNear(10)
      val p = FrustumPinholeProjection(fr)
      val nP = FrustumPinholeProjection(nF)
      p2d(nP(x3d)) shouldBe p2d(p(x3d))
    }

    describe("should be movable off-center") {
      val c = Point(rnd.scalaRandom.nextDouble() * 2 - 1, rnd.scalaRandom.nextDouble() * 2 - 1)
      val ocF = fr.withCenter(c)

      it("with a proper new center") {
        val vCenter = ocF.center
        vCenter shouldBe c
      }

      it("with shifted projections") {
        val ph = FrustumPinholeProjection(fr)
        val or = FrustumOrthographicProjection(fr)
        val ocph = FrustumPinholeProjection(ocF)
        val ocor = FrustumOrthographicProjection(ocF)
        val xph = ph(x3d)
        val xor = or(x3d)
        val xocph = ocph(x3d)
        val xocor = ocor(x3d)
        p2d(xocor) - c.toVector shouldBe p2d(xor)
        p2d(xocph) - c.toVector shouldBe p2d(xph)
      }

      it("can be re-centered") {
        ocF.centered shouldBe fr
      }
    }
  }

  describe("FrustumPinholeProjection") {
    val p = FrustumPinholeProjection(frustum)

    it("is properly inverted") {
      val x3d = Point(rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 10e3 + 1e3)
      val x3dp = p.inverse(p(x3d))
      (x3d - x3dp).norm should be < 1.0
    }

    it("works properly for known points") {
      val pointsWithTarget = IndexedSeq(
        (Point(0f, 0f, -2e3), Point(0f, 0f, 0.515151515)),
        (Point(100f, -50f, -1.5e3), Point(0.1904762, -0.12820514, 0.34680134))
      )
      pointsWithTarget.foreach {
        case (x, target) =>
          (p(x) - target).norm should be < 1.0
      }
    }

    it("provides a result which is consistent with homogeneous coordinates") {
      val m4 = p.matrix4
      val x = Point(rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 10e3 + 1e3)
      val xh: DenseVector[Double] = DenseVector(x.x, x.y, x.z, 1.0)
      val xhp: DenseVector[Double] = m4 * xh
      val xp = Point(xhp(0) / xhp(3), xhp(1) / xhp(3), xhp(2) / xhp(3))
      (p(x) - xp).norm should be < 1.0
    }
  }

  describe("FrustumOrthographicProjection") {
    val p = FrustumOrthographicProjection(frustum)

    it("is properly inverted") {
      val x3d = Point(rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 10e3 + 1e3)
      val x3dp = p.inverse(p(x3d))
      (x3d - x3dp).norm should be < 1.0
    }

    it("works properly for known points") {
      val pointsWithTarget = IndexedSeq(
        (Point(0f, 0f, -2e3), Point(0.0, 0.0, -0.93939394)),
        (Point(100f, -50f, -1.5e3), Point(0.5714286, -0.3846154, -0.959596))
      )

      val projected = pointsWithTarget.map(x => (p(x._1), x._2))

      projected.foreach {
        case (x, px) =>
          (x - px).norm should be < 1.0
      }
    }

    it("provides a result which is consistent with homogeneous coordinates") {
      val m4 = p.matrix4
      val x = Point(rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 1e3, rnd.scalaRandom.nextDouble() * 10e3 + 1e3)
      val xh: DenseVector[Double] = DenseVector(x.x, x.y, x.z, 1.0)
      val xhp: DenseVector[Double] = m4 * xh
      val xp = Point(xhp(0) / xhp(3), xhp(1) / xhp(3), xhp(2) / xhp(3))
      (p(x) - xp).norm should be < 1.0
    }

    it("gives an identical x,y result as the projective projection at a constant depth (and adjusted frustum)") {
      val z = -1.5e3
      val fp = frustum.closestOrthographic(math.abs(z))

      val perspP = FrustumPinholeProjection(frustum)
      val closestP = FrustumOrthographicProjection(fp)

      val x = Point(10f, -5f, -1.5e3)

      val pxPersp = perspP(x)
      val pxOrtho = closestP(x)

      (p2d(pxPersp) - p2d(pxOrtho)).norm should be < 1.0
    }
  }
}
