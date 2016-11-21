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

package scalismo.faces.image

import scala.math._

/** kernel as used for interpolation ("continuously" normalized) */
trait InterpolationKernel {
  def apply(x: Double): Double
  def radius: Double
}

object InterpolationKernel {
  case class BoxKernel(size: Double) extends InterpolationKernel {
    override val radius = size

    override def apply(x: Double) = if (math.abs(x) <= size) 1.0 / (2.0 * size) else 0.0
  }

  case class LanczosKernel(window: Int) extends InterpolationKernel {
    override def apply(x: Double): Double = {
      if (x == 0.0) 1.0
      else if (abs(x) >= window) 0.0
      else sinc(x) * sinc(x / window)
    }

    private def sinc(x: Double): Double = sin(x * math.Pi) / (x * math.Pi)

    override def radius = window
  }

  case class GaussKernel(sigma: Double) extends InterpolationKernel {
    override val radius = 4 * sigma
    private val n = math.sqrt(2 * math.Pi) / sigma

    override def apply(x: Double): Double = math.exp(-0.5 * x * x / sigma / sigma) / n
  }

  case class DiscreteKernel(kernel: IndexedSeq[Double]) extends InterpolationKernel {
    require(kernel.size % 2 == 1, "need an odd-sized kernel")

    override def apply(x: Double): Double = {
      val cell = x.toInt + radius.toInt
      if (cell > kernel.size - 1 || cell < 0) 0.0
      else kernel(cell)
    }

    override def radius = (kernel.size - 1) / 2
  }

  /**
   * cubic interpolation kernel, Mitchell-Netravali,
   * Catmull-Rom: b=0, c=0.5 (GIMP)
   * Cubic b-spline: b=1, c=0
   * Cardinal splines: b=0
   * Good values: b=1/3, c=1/3
   *
   * @param b Parameter, e.g. 1/3
   * @param c Parameter, e.g. 1/3
   */
  case class CubicKernel(b: Double, c: Double) extends InterpolationKernel {
    override def apply(x: Double): Double = {
      val x1 = math.abs(x)
      val x2 = x1 * x1
      val x3 = x2 * x1
      if (x1 <= 1.0)
        (12 - 9 * b - 6 * c) * x3 + (-18 + 12 * b + 6 * c) * x2 + 6 - 2 * b
      else if (x1 < 2.0)
        (-b - 6 * c) * x3 + (6 * b + 30 * c) * x2 + (-12 * b - 48 * c) * x1 + 8 * b + 24 * c
      else
        0.0
    }

    override def radius = 2.0
  }

  object CubicKernel {
    val catmullRom = CubicKernel(0.0, 0.5) // GIMP ("cubic")
    val bSpline = CubicKernel(0.0, 1.0)
    val mitchellNetravali = CubicKernel(1.0 / 3, 1.0 / 3)
  }

  case object BilinearKernel extends InterpolationKernel {
    override val radius = 1.0

    override def apply(x: Double) = {
      if (math.abs(x) <= 1.0)
        if (x > 0) 1.0 - x else x + 1.0
      else
        0.0
    }
  }
}