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
import scalismo.geometry._

/** quaternion to handle rotation */
case class Quaternion(r: Double, v: EuclideanVector[_3D]) {
  def norm: Double = math.sqrt(r*r + v.norm2)

  def normalize: Quaternion = this / norm

  def *(other: Quaternion): Quaternion = {
    val Quaternion(s, w) = other
    Quaternion(r * s - v.dot(w), r *: w + s *: v + v.crossproduct(w))
  }

  def *(scale: Double) = Quaternion(r * scale, v * scale)

  def /(scale: Double) = Quaternion(r / scale, v / scale)

  def *:(scale: Double): Quaternion = this * scale

  def conjugate = Quaternion(r, -v)

  def unary_- = Quaternion(-r, -v)

  def dot(other: Quaternion): Double = r * other.r + v.dot(other.v)
}

/** rotation in 3D in angle/axis parameterization */
case class Rotation3D(phi: Double, axis: EuclideanVector[_3D]) extends InvertibleTransform3D with Transform4x4 {
  private val normalizedAxis = axis.normalize // axis must be normalized

  /** quaternion describing this rotation */
  lazy val quaternion: Quaternion = Quaternion(math.cos(phi / 2.0), math.sin(phi / 2.0) *: normalizedAxis)

  /** rotation matrix describing action in 3D space */
  val rotationMatrix: SquareMatrix[_3D] = {
    val c = math.cos(phi)
    val s = math.sin(phi)
    val uu = normalizedAxis.outer(normalizedAxis)
    val ux = SquareMatrix((0.0, -normalizedAxis.z, normalizedAxis.y), (normalizedAxis.z, 0.0, -normalizedAxis.x), (-normalizedAxis.y, normalizedAxis.x, 0.0))
    SquareMatrix.eye[_3D] * c + ux * s + uu * (1.0 - c)
  }

  /** rotate point */
  override def apply(x: Point[_3D]): Point[_3D] = (rotationMatrix * x.toVector).toPoint

  /** rotate vector */
  override def apply(v: EuclideanVector[_3D]): EuclideanVector[_3D] = rotationMatrix * v

  /** inverted rotation */
  override def inverted: Rotation3D = Rotation3D(-phi, normalizedAxis)

  /** compose with other rotation */
  def compose(other: Rotation3D): Rotation3D = {
    Rotation3D.fromQuaternion(quaternion * other.quaternion)
  }

  /** compose as affine transform */
  def compose(other: Affine3D): Affine3D = toAffine3D compose other

  /** rotation matrix expressed in homogenuous coordinates */
  override def matrix4: DenseMatrix[Double] =
    DenseMatrix.vertcat(
      DenseMatrix.horzcat(
        rotationMatrix.toBreezeMatrix,
        DenseMatrix(0.0, 0.0, 0.0)),
      DenseMatrix((0.0, 0.0, 0.0, 1.0))
    )

  def toAffine3D: Affine3D = Affine3D(rotationMatrix, EuclideanVector(0.0, 0.0, 0.0))
}

object Rotation3D {
  /** build rotation from quaternion */
  def fromQuaternion(q: Quaternion): Rotation3D = {
    val Quaternion(r: Double, v: EuclideanVector[_3D]) = q
    val phi = 2 * math.atan2(v.norm, r)
    val axis = if (phi != 0.0) v / math.sin(phi / 2.0) else EuclideanVector3D.unitX
    Rotation3D(phi, axis)
  }

  /** build rotation from Euler angles */
  def fromEulerXYZ(x: Double, y: Double, z: Double): Rotation3D = {
    rotationZ(z) compose rotationY(y) compose rotationX(x)
  }

  def rotationX(phi: Double) = Rotation3D(phi, EuclideanVector3D.unitX)

  def rotationY(phi: Double) = Rotation3D(phi, EuclideanVector3D.unitY)

  def rotationZ(phi: Double) = Rotation3D(phi, EuclideanVector3D.unitZ)

  /** find Euler angles for given rotation */
  def decomposeRotationXYZ(rotation: Rotation3D): (Double, Double, Double) = {
    val r = rotation.rotationMatrix
    val pitch = math.atan2(r(2, 1), r(2, 2))
    val yaw = math.atan2(-r(2, 0), math.sqrt(r(2, 1) * r(2, 1) + r(2, 2) * r(2, 2)))
    val roll = math.atan2(r(1, 0), r(0, 0))
    (pitch, yaw, roll)
  }
}
