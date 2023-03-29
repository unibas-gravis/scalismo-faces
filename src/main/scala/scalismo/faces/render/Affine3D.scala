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
import scalismo.geometry.{_3D, EuclideanVector, Point, SquareMatrix}

/** affine transform in 3D */
case class Affine3D(A: SquareMatrix[_3D], b: EuclideanVector[_3D]) extends InvertibleTransform3D with Transform4x4 {

  /** inverted version of this transform */
  override def inverted: Affine3D = {
    val Ainv = SquareMatrix.inv(A)
    Affine3D(Ainv, Affine3D.mult(Ainv, -b))
  }

  /** apply transform to a 3d point */
  override def apply(x: Point[_3D]): Point[_3D] = (Affine3D.mult(A, x.toVector) + b).toPoint

  /** apply transform to a 3d vector */
  override def apply(v: EuclideanVector[_3D]): EuclideanVector[_3D] = Affine3D.mult(A, v)

  override def matrix4: DenseMatrix[Double] =
    DenseMatrix.vertcat(
      DenseMatrix.horzcat(A.toBreezeMatrix, DenseMatrix(b.x, b.y, b.z)),
      DenseMatrix((0.0, 0.0, 0.0, 1.0))
    )

  /** compose with other invertible transform */
  def compose(other: Affine3D): Affine3D = Affine3D(A * other.A, Affine3D.mult(A, other.b) + b)
}

object Affine3D {
  def apply(rotation: Rotation3D): Affine3D = rotation.toAffine3D
  def apply(translation3D: Translation3D): Affine3D = translation3D.toAffine3D
  def apply(scaling3D: Scaling3D): Affine3D = scaling3D.toAffine3D

  /** fast matrix multiplication for 3D (scalismo generic is not optimal) */
  @inline
  private def mult(m: SquareMatrix[_3D], v: EuclideanVector[_3D]): EuclideanVector[_3D] = {
    EuclideanVector(m(0, 0) * v.x + m(0, 1) * v.y + m(0, 2) * v.z,
                    m(1, 0) * v.x + m(1, 1) * v.y + m(1, 2) * v.z,
                    m(2, 0) * v.x + m(2, 1) * v.y + m(2, 2) * v.z
    )
  }
}
