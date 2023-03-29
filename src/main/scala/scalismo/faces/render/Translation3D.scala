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

/** 3D translation */
case class Translation3D(t: EuclideanVector[_3D]) extends InvertibleTransform3D with Transform4x4 {
  override def apply(x: Point[_3D]): Point[_3D] = x + t
  override def apply(v: EuclideanVector[_3D]): EuclideanVector[_3D] = v // a vector is not changed by a translation

  override def inverted: Translation3D = Translation3D(-t)

  def compose(other: Translation3D) = Translation3D(t + other.t)

  override def matrix4 = DenseMatrix(
    (1.0, 0.0, 0.0, t.x),
    (0.0, 1.0, 0.0, t.y),
    (0.0, 0.0, 1.0, t.z),
    (0.0, 0.0, 0.0, 1.0)
  )

  def toAffine3D: Affine3D = Affine3D(SquareMatrix.eye[_3D], t)
}
