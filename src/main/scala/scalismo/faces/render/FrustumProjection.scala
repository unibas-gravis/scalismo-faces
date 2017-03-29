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

import breeze.linalg.DenseMatrix
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.BarycentricCoordinates

/** geometric projection to create an image of a 3D scene */
trait Projection {
  /** apply the projection, returns normalized device coordinates [-1,1]x[-1,1]x[-1,1] */
  def apply(p: Point[_3D]): Point[_3D]

  def pointShader(modelView: Transform3D): PointShader

  /** viewing ray starting from given point in NDC */
  def ray(x: Double, y: Double): (Double => Point[_3D]) = z => inverse(Point(x, y, z))

  /** inverse projection into eye space */
  def inverse(point: Point[_3D]): Point[_3D]
}

/** projection described by a frustum */
trait FrustumProjection extends Projection {
  def frustum: Frustum
}

/** project a scene onto the image plane: perspective */
case class FrustumPinholeProjection(override val frustum: Frustum) extends FrustumProjection with Transform4x4 {
  self =>

  private val Frustum(l, r, b, t, n, f) = frustum

  /** apply transform to a 3d point */
  override def apply(p: Point[_3D]): Point[_3D] = {
    val d = -p.z
    Point(
      (p.x * 2 * n / (r - l) + (r + l) / (r - l) * p.z) / d,
      (p.y * 2 * n / (t - b) + (t + b) / (t - b) * p.z) / d,
      (-(f + n) / (f - n) * p.z - 2 * f * n / (f - n)) / d)
  }

  override val matrix4: DenseMatrix[Double] = DenseMatrix(
    (2 * n / (r - l), 0.0, (r + l) / (r - l), 0.0),
    (0.0, 2 * n / (t - b), (t + b) / (t - b), 0.0),
    (0.0, 0.0, -(f + n) / (f - n), -2 * f * n / (f - n)),
    (0.0, 0.0, -1.0, 0.0)
  )

  /** create a point shader with a given model view transform and this projection, does correct BCC appropriately */
  override def pointShader(modelView: Transform3D): PointShader = {
    // create a po
    new PointShader {
      def invZ(p: Point[_3D]): Double = 1.0 / ((p.z - (f + n) / (f - n)) * (f - n) / (2 * f * n))

      override def apply(p: Point[_3D]): Point[_3D] = self(modelView(p))

      override def bccScreenToWorld(screenBCC: BarycentricCoordinates, a: Point[_3D], b: Point[_3D], c: Point[_3D]): BarycentricCoordinates = {
        FrustumPinholeProjection.bccScreenToWorldCorrection(screenBCC, invZ(a), invZ(b), invZ(c))
      }

      override def bccWorldToScreen(worldBCC: BarycentricCoordinates, a: Point[_3D], b: Point[_3D], c: Point[_3D]): BarycentricCoordinates = {
        FrustumPinholeProjection.bccWorldToScreenCorrection(worldBCC, invZ(a), invZ(b), invZ(c))
      }
    }
  }

  /** inverse projection into eye space */
  override def inverse(p: Point[_3D]): Point[_3D] = {
    val z = 1.0 / ((p.z - (f + n) / (f - n)) * (f - n) / (2 * f * n))
    Point(
      -z * (r - l) / (2 * n) * (p.x + (r + l) / (r - l)),
      -z * (t - b) / (2 * n) * (p.y + (t + b) / (t - b)),
      z)
  }
}

object FrustumPinholeProjection {
  def bccScreenToWorldCorrection(bccScreen: BarycentricCoordinates, z1: Double, z2: Double, z3: Double): BarycentricCoordinates = {
    val d = z2 * z3 + z3 * bccScreen.b * (z1 - z2) + z2 * bccScreen.c * (z1 - z3)
    if (d == 0.0) {
      bccScreen
    } else {
      val b = z1 * z3 * bccScreen.b / d
      val c = z1 * z2 * bccScreen.c / d
      BarycentricCoordinates(1.0 - b - c, b, c) //lambda world
    }
  }

  def bccWorldToScreenCorrection(bccWorld: BarycentricCoordinates, z1: Double, z2: Double, z3: Double): BarycentricCoordinates = {
    val d = z1 - (z1 - z2) * bccWorld.b - (z1 - z3) * bccWorld.c
    if (d == 0.0)
      bccWorld
    else {
      val c = bccWorld.c * z3 / d
      val b = bccWorld.b * z2 / d
      BarycentricCoordinates(1.0 - b - c, b, c)
    }
  }
}

/** project a scene onto the image plane: orthographic */
case class FrustumOrthographicProjection(override val frustum: Frustum) extends FrustumProjection with Transform4x4 {
  self =>

  private val Frustum(l, r, b, t, n, f) = frustum

  /** apply transform to a 3d point */
  override def apply(p: Point[_3D]): Point[_3D] = Point(
    p.x * 2 / (r - l) - (r + l) / (r - l),
    p.y * 2 / (t - b) - (t + b) / (t - b),
    -2 / (f - n) * p.z - (f + n) / (f - n))

  override val matrix4: DenseMatrix[Double] = DenseMatrix(
    (2 / (r - l), 0.0, 0.0, -(r + l) / (r - l)),
    (0.0, 2 / (t - b), 0.0, -(t + b) / (t - b)),
    (0.0, 0.0, -2 / (f - n), -(f + n) / (f - n)),
    (0.0, 0.0, 0.0, 1.0)
  )

  /** inverse projection into eye space */
  override def inverse(p: Point[_3D]): Point[_3D] = Point(
    (r - l) / 2 * (p.x + (r + l) / (r - l)),
    (t - b) / 2 * (p.y + (t + b) / (t - b)),
    (f - n) / 2 * (-p.z - (f + n) / (f - n))
  )

  override def pointShader(modelView: Transform3D) = new PointShader {
    override def apply(p: Point[_3D]): Point[_3D] = self(modelView(p))
  }
}
