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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.geometry._

trait Transform3D { self =>
  /** apply transform to a 3d point */
  def apply(x: Point[_3D]): Point[_3D]
  /** apply transform to a 3d vector */
  def apply(v: Vector[_3D]): Vector[_3D]
  /** compose with other transform */
  def compose(t: Transform3D): Transform3D = new Transform3D {
    override def apply(x: Point[_3D]): Point[_3D] = self(t(x))
    override def apply(v: Vector[_3D]): Vector[_3D] = self(t(v))
  }
}

object Transform3D {
  val identity = new Transform3D {
    override def apply(x: Point[_3D]): Point[_3D] = x
    override def apply(v: Vector[_3D]): Vector[_3D] = v
  }
}

trait InvertibleTransform3D extends Transform3D { self: Transform3D =>
  /** inverted version of this transform */
  def inverted: InvertibleTransform3D

  /** compose with other invertible transform */
  def compose(u: InvertibleTransform3D): InvertibleTransform3D = {
    val t = self
    new InvertibleTransform3D {
      thisTransform =>
      override def apply(x: Point[_3D]): Point[_3D] = t(u(x))
      override def apply(v: Vector[_3D]): Vector[_3D] = t(u(v))
      override def inverted = new InvertibleTransform3D {
        override def apply(x: Point[_3D]): Point[_3D] = u.inverted(t.inverted(x))
        override def apply(v: Vector[_3D]): Vector[_3D] = u.inverted(t.inverted(v))
        override def inverted: InvertibleTransform3D {
          def inverted: InvertibleTransform3D with Object {
            def inverted: Any

            def apply(v: Vector[_3D]): Vector[_3D]

            def apply(x: Point[_3D]): Point[_3D]
          }

          def apply(v: Vector[_3D]): Vector[_3D]

          def apply(x: Point[_3D]): Point[_3D]
        } = thisTransform
      }
    }
  }
}

trait Transform4x4 {
  def matrix4: DenseMatrix[Double]

  def compose(other: Transform4x4): Transform4x4 = Matrix4Transform(matrix4 * other.matrix4)
}

case class Matrix4Transform(override val matrix4: DenseMatrix[Double]) extends Transform4x4 with InvertibleTransform3D {
  require(matrix4.rows == 4 && matrix4.cols == 4, "transformation matrix must 4x4 (homogenuous coordinates)")

  override def apply(p: Point[_3D]): Point[_3D] = {
    val p4 = DenseVector(p.x, p.y, p.z, 1.0)
    val tp = matrix4 * p4
    Point(tp(0) / tp(3), tp(1) / tp(3), tp(2) / tp(3))
  }

  override def apply(v: Vector[_3D]): Vector[_3D] = {
    val v4 = DenseVector(v.x, v.y, v.z, 0.0)
    val tp = matrix4 * v4
    Vector(tp(0), tp(1), tp(2))
  }

  /** inverted version of this transform */
  override def inverted: InvertibleTransform3D = Matrix4Transform(breeze.linalg.inv(matrix4))
}
