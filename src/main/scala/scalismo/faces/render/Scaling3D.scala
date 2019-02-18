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
import scalismo.geometry.{Point, SquareMatrix, EuclideanVector, _3D}

/** scaling transfrom in 3D */
case class Scaling3D(fx: Double, fy: Double, fz: Double) extends InvertibleTransform3D with Transform4x4 {

  override def apply(x: Point[_3D]): Point[_3D] = this(x.toVector).toPoint
  override def apply(v: EuclideanVector[_3D]): EuclideanVector[_3D] = EuclideanVector(v.x * fx, v.y * fy, v.z * fz)

  override def inverted: Scaling3D = Scaling3D(1.0 / fx, 1.0 / fy, 1.0 / fz)

  def compose(other: Scaling3D): Scaling3D = Scaling3D(other.fx * fx, other.fy * fy, other.fz * fz)

  override def matrix4 = DenseMatrix(
    (fx, 0.0, 0.0, 0.0),
    (0.0, fy, 0.0, 0.0),
    (0.0, 0.0, fz, 0.0),
    (0.0, 0.0, 0.0, 1.0)
  )

  def toAffine3D: Affine3D = Affine3D(
    SquareMatrix(
      (fx, 0.0, 0.0),
      (0.0, fy, 0.0),
      (0.0, 0.0, fz)),
    EuclideanVector(0.0, 0.0, 0.0)
  )
}

object Scaling3D {
  /** isotropic scaling */
  def apply(s: Double) = new Scaling3D(s, s, s)
}
