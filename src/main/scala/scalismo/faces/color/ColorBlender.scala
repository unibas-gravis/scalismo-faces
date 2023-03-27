/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.color

import scalismo.color.ColorSpaceOperations
import scalismo.geometry._

import scala.annotation.tailrec

/** Defines a homotopy, mixing between two entities, e.g. colors (mixing is a convex combination of elements) */
trait ColorBlender[@specialized(Double,Float) A] {
  /** Blend two colors (or other objects), l is within [0, 1] */
  def blend(obj1: A, obj2: A, l: Double): A

  /** blend multiple values in a convex linear combination */
  @tailrec
  final def convexCombination(first: (A, Double), rest: (A, Double)*): A = {
    if (rest.nonEmpty) {
      val (v, f: Double) = first
      val (v1, f1: Double) = rest.head
      val blended = if (f + f1 > 0.0) blend(v, v1, f / (f + f1)) else v
      convexCombination((blended, f + f1), rest.tail: _*) // Seq to varargs: _*
    } else
      first._1
  }
}

object ColorBlender {
  implicit val floatBlender: ColorBlender[Float] = (obj1: Float, obj2: Float, l: Double) => (obj1 * l + obj2 * (1.0 - l)).toFloat

  implicit val doubleBlender: ColorBlender[Double] = (obj1: Double, obj2: Double, l: Double) => obj1 * l + obj2 * (1.0 - l)

  implicit val point3DBlender: ColorBlender[Point[`_3D`]] = (obj1: Point[_3D], obj2: Point[_3D], l: Double) => obj1 + (1.0 - l) *: (obj2 - obj1)

  implicit val point2DBlender: ColorBlender[Point[`_2D`]] = (obj1: Point[_2D], obj2: Point[_2D], l: Double) => obj1 + (1.0 - l) *: (obj2 - obj1)

  implicit def pointBlender[D <: Dim]: ColorBlender[Point[D]] = (obj1: Point[D], obj2: Point[D], l: Double) => obj1 + (1.0 - l) *: (obj2 - obj1)

  /** implicit construction of ColorBlender from more powerful ColorSpaceOperations */
  implicit def fromColorSpace[A](implicit space: ColorSpaceOperations[A]): ColorBlender[A] = (obj1: A, obj2: A, l: Double) => space.blend(obj1, obj2, l)

  /** implicit construction of a blender for A wrapped in Option[A] */
  implicit def optionBlender[A](implicit blender: ColorBlender[A]): ColorBlender[Option[A]] = (obj1: Option[A], obj2: Option[A], l: Double) => {
    (obj1, obj2) match {
      case (Some(o1), Some(o2)) => Some(blender.blend(o1, o2, l))
      case (Some(o1), None) => Some(o1)
      case (None, Some(o2)) => Some(o2)
      case (None, None) => None
    }
  }
}




