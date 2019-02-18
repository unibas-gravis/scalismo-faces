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

import scalismo.geometry._
import scalismo.mesh.BarycentricCoordinates

object RenderTransforms {
  /** default model transform, consisting of scaling, 3 (Euler) rotations (x, y, z) and translation */
  def modelTransform(translation: EuclideanVector[_3D], scaling: Double, pitch: Double, yaw: Double, roll: Double): Affine3D = {
    val rotX = Rotation3D.rotationX(pitch)
    val rotY = Rotation3D.rotationY(yaw)
    val rotZ = Rotation3D.rotationZ(roll)
    val t = Translation3D(translation)
    val scale = Scaling3D(scaling)
    t.toAffine3D compose (rotZ compose rotY compose rotX).toAffine3D compose scale.toAffine3D
  }

  /**
   * camera/view transform to look at a certain point, see gluLookAt
   *
   * @param position position of the camera center
   * @param lookAt   center point of the camera, looks at given point
   * @param upright  upwards direction of camera
   */
  def viewTransformLookAt(position: Point[_3D], lookAt: Point[_3D], upright: EuclideanVector[_3D]): Affine3D = {
    // rotations according to lookAt and upright, as in gluLookAt
    val f = (lookAt - position).normalize
    val up = upright.normalize
    val s = (f crossproduct up).normalize
    val u = (s crossproduct f).normalize
    val M = SquareMatrix(
      (s.x, s.y, s.z),
      (u.x, u.y, u.z),
      (-f.x, -f.y, -f.z)
    )
    // translation: camera center
    val p = position.toVector
    val t = EuclideanVector(-s dot p, -u dot p, f dot p)
    Affine3D(M, t)
  }

  /** default camera/view transform, consists of three rotations (Euler, xyz) and a translation of the camera center (inverts) */
  def viewTransform(translation: EuclideanVector[_3D], pitch: Double, yaw: Double, roll: Double): Affine3D = {
    val rotX = Rotation3D.rotationX(pitch)
    val rotY = Rotation3D.rotationY(yaw)
    val rotZ = Rotation3D.rotationZ(roll)
    val t = Translation3D(translation)
    (t.toAffine3D compose (rotZ compose rotY compose rotX).toAffine3D).inverted
  }

  /** default correction of barycentric coordinates in screen space to world (3d) space for a perspective projection */
  def bccScreenToWorldCorrectionPerspective(bccScreen: BarycentricCoordinates, z1: Double, z2: Double, z3: Double): BarycentricCoordinates = {
    val d = z2 * z3 + z3 * bccScreen.b * (z1 - z2) + z2 * bccScreen.c * (z1 - z3)
    if (d == 0f) {
      bccScreen
    } else {
      val b = z1 * z3 * bccScreen.b / d
      val c = z1 * z2 * bccScreen.c / d
      BarycentricCoordinates(1f - b - c, b, c) //lambda world
    }
  }

  /** default correction of barycentric coordinates from world (3d) space to screen space for a perspective projection */
  def bccWorldToScreenCorrectionPerspective(bccWorld: BarycentricCoordinates, z1: Double, z2: Double, z3: Double): BarycentricCoordinates = {
    val d = z1 - (z1 - z2) * bccWorld.b - (z1 - z3) * bccWorld.c
    if (d == 0f)
      bccWorld
    else {
      val c = bccWorld.c * z3 / d
      val b = bccWorld.b * z2 / d
      BarycentricCoordinates(1f - b - c, b, c)
    }
  }
}

