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

package scalismo.faces.numerics

import scalismo.geometry.{Vector, _3D}

import scala.annotation.switch
import scala.math._

/** two-values index of Spherical Harmonics function (l, m) */
case class SHIndex(l: Int, m: Int) {
  require(isValid, s"SHIndex is invalid! ($l, $m)")
  def isValid: Boolean = abs(m) <= l
  val toSingleIndex: Int = l * l + m + l
}

object SHIndex {
  def apply(i: Int): SHIndex = {
    val l = sqrt(i).toInt
    val m = i - l * l - l
    SHIndex(l, m)
  }
}

/** service functions for real spherical harmonics */
object SphericalHarmonics {
  type SHBasisFunction = ((Vector[_3D]) => Double)

  val N0: Double = sqrt(1.0 / Pi) / 2.0

  val N1: Double = sqrt(3.0 / Pi) / 2.0

  val N2_2: Double = sqrt(15.0 / Pi) / 4.0
  val N2_1: Double = sqrt(15.0 / Pi) / 2.0
  val N2_0: Double = sqrt(5.0 / Pi) / 4.0

  val N3_3: Double = sqrt(35.0 / 2.0 / Pi) / 4.0
  val N3_2: Double = sqrt(105.0 / Pi) / 2.0
  val N3_1: Double = sqrt(21.0 / 2.0 / Pi) / 4.0
  val N3_0: Double = sqrt(7.0 / Pi) / 4.0

  val N4_4: Double = sqrt(35.0 / Pi) * 3.0 / 16.0
  val N4_3: Double = sqrt(35.0 / 2.0 / Pi) * 3.0 / 4.0
  val N4_2: Double = sqrt(5.0 / Pi) * 3.0 / 8.0
  val N4_1: Double = sqrt(5.0 / 2.0 / Pi) * 3.0 / 4.0
  val N4_0: Double = sqrt(1.0 / Pi) * 3.0 / 16.0

  //https://en.wikipedia.org/wiki/Table_of_spherical_harmonics#Real_spherical_harmonics
  private val shBasisPredefined = Map[SHIndex, SHBasisFunction](
    SHIndex(0, 0) -> ((v: Vector[_3D]) => N0),

    SHIndex(1, -1) -> ((v: Vector[_3D]) => N1 * v.y),
    SHIndex(1, 0) -> ((v: Vector[_3D]) => N1 * v.z),
    SHIndex(1, 1) -> ((v: Vector[_3D]) => N1 * v.x),

    SHIndex(2, -2) -> ((v: Vector[_3D]) => N2_1 * v.x * v.y),
    SHIndex(2, -1) -> ((v: Vector[_3D]) => N2_1 * v.y * v.z),
    SHIndex(2, 0) -> ((v: Vector[_3D]) => N2_0 * (2 * v.z * v.z - v.x * v.x - v.y * v.y)),
    SHIndex(2, 1) -> ((v: Vector[_3D]) => N2_1 * v.z * v.x),
    SHIndex(2, 2) -> ((v: Vector[_3D]) => N2_2 * (v.x * v.x - v.y * v.y)),

    SHIndex(3, -3) -> ((v: Vector[_3D]) => N3_3 * (3 * v.x * v.x - v.y * v.y) * v.y),
    SHIndex(3, -2) -> ((v: Vector[_3D]) => N3_2 * (v.x * v.y * v.z)),
    SHIndex(3, -1) -> ((v: Vector[_3D]) => N3_1 * (4 * v.z * v.z - v.x * v.x - v.y * v.y) * v.y),
    SHIndex(3, 0) -> ((v: Vector[_3D]) => N3_0 * (2 * v.z * v.z - 3 * v.x * v.x - 3 * v.y * v.y) * v.z),
    SHIndex(3, 1) -> ((v: Vector[_3D]) => N3_1 * (4 * v.z * v.z - v.x * v.x - v.y * v.y) * v.x),
    SHIndex(3, 2) -> ((v: Vector[_3D]) => N3_2 * (v.x * v.x - v.y * v.y) * v.z),
    SHIndex(3, 3) -> ((v: Vector[_3D]) => N3_3 * (v.x * v.x - 3 * v.y * v.y) * v.x),

    SHIndex(4, -4) -> ((v: Vector[_3D]) => N4_4 * 4 * v.x * v.y * (v.x * v.x - v.y * v.y)),
    SHIndex(4, -3) -> ((v: Vector[_3D]) => N4_3 * (3 * v.x * v.x - v.y * v.y) * v.y * v.z),
    SHIndex(4, -2) -> ((v: Vector[_3D]) => N4_2 * 2 * v.x * v.y * (7 * v.z * v.z - 1.0)),
    SHIndex(4, -1) -> ((v: Vector[_3D]) => N4_1 * v.y * v.z * (7 * v.z * v.z - 3.0)),
    SHIndex(4, 0) -> ((v: Vector[_3D]) => N4_0 * (35 * v.z * v.z * v.z * v.z - 30 * v.z * v.z + 3.0)),
    SHIndex(4, 1) -> ((v: Vector[_3D]) => N4_1 * v.x * v.z * (7 * v.z * v.z - 3.0)),
    SHIndex(4, 2) -> ((v: Vector[_3D]) => N4_2 * (v.x * v.x - v.y * v.y) * (7 * v.z * v.z - 1.0)),
    SHIndex(4, 3) -> ((v: Vector[_3D]) => N4_3 * v.x * v.z * (v.x * v.x - 3 * v.y * v.y)),
    SHIndex(4, 4) -> ((v: Vector[_3D]) => N4_4 * (v.x * v.x * (v.x * v.x - 3 * v.y * v.y) - v.y * v.y * (3 * v.x * v.x - v.y * v.y)))
  )

  /** analytic fall-back to get an arbitrary SH basis function ... slow! */
  private def shBasisFunctionAnalytic(ind: SHIndex): SHBasisFunction = ???

  /** back predefined SH basis with slower analytic solution for every index */
  private val shBasisFunctionFull = shBasisPredefined.withDefault(shBasisFunctionAnalytic)

  /** get proper function and wrap with a normalization */
  private def shBasisFunctionFromMap(index: SHIndex): SHBasisFunction = {
    def norm(v: Vector[_3D]): Vector[_3D] = v.normalize
    val sh = shBasisFunctionFull(index)
    // work with normalized vector - direction
    (v: Vector[_3D]) => sh(norm(v))
  }

  /** cache SH basis in fast Vector */
  private val shBasisPredefinedTabulated: IndexedSeq[SHBasisFunction] = {
    IndexedSeq.tabulate(shBasisPredefined.size) { i => shBasisFunctionFromMap(SHIndex(i)) }
  }

  // lookup table for l and m as function of a linear index i
  private val lLookup: Array[Int] = Array.tabulate(totalCoefficients(4)) { i => sqrt(i).toInt }
  private def lFromIndex(i: Int): Int = if (i < lLookup.length) lLookup(i) else sqrt(i).toInt

  /** get SH basis function with given indices l, m - fast for l < 5 */
  def shBasisFunction(l: Int, m: Int): SHBasisFunction = shBasisFunction(SHIndex(l, m))

  /** get SH basis function with given index - fast for l < 5 */
  def shBasisFunction(shi: SHIndex): SHBasisFunction = {
    val i = shi.toSingleIndex
    if (shBasisPredefinedTabulated.isDefinedAt(i))
      shBasisPredefinedTabulated(i)
    else
      shBasisFunctionFromMap(shi)
  }

  /** get SH basis function with linear index - fast for i < 25 */
  def shBasisFunction(i: Int): SHBasisFunction = {
    if (shBasisPredefinedTabulated.isDefinedAt(i))
      shBasisPredefinedTabulated(i)
    else
      shBasisFunctionFromMap(SHIndex(i))
  }

  /** total number of functions when using expansions with l bands */
  def totalCoefficients(bands: Int): Int = (bands + 1) * (bands + 1)

  /** number of m values within a band l */
  def coefficientsInBand(band: Int): Int = 2 * band + 1

  /** number of l values required to have given number of coefficients */
  def numberOfBandsForCoefficients(coefficients: Int): Int = sqrt(coefficients - 1).toInt

  /** direction calculation of Spherical Harmonics basis function */
  def shBasisFunctionDirect(i: Int, direction: Vector[_3D]): Double = {
    val l = lFromIndex(i)
    val m = i - l * l - l
    shBasisFunctionDirect(l, m, direction)
  }

  /** direct calculation of SH basis function, only for l < 5 (c++ nostalgia version) */
  def shBasisFunctionDirect(l: Int, m: Int, direction: Vector[_3D]): Double = {
    //https://en.wikipedia.org/wiki/Table_of_spherical_harmonics#Real_spherical_harmonics
    val v: Vector[_3D] = direction.normalize

    val x = v.x
    val y = v.y
    val z = v.z

    val xx = x * x
    val yy = y * y
    val zz = z * z

    (l: @switch) match {
      case 0 => N0
      case 1 => m match {
        case -1 => N1 * y
        case 0 => N1 * z
        case 1 => N1 * x
      }
      case 2 => m match {
        case -2 => N2_1 * x * y
        case -1 => N2_1 * y * z
        case 0 => N2_0 * (2 * zz - xx - yy)
        case 1 => N2_1 * z * x
        case 2 => N2_2 * (xx - yy)
      }
      case 3 => m match {
        case -3 => N3_3 * (3 * xx - yy) * y
        case -2 => N3_2 * (x * y * z)
        case -1 => N3_1 * (4 * zz - xx - yy) * y
        case 0 => N3_0 * (2 * zz - 3 * xx - 3 * yy) * z
        case 1 => N3_1 * (4 * zz - xx - yy) * x
        case 2 => N3_2 * (xx - yy) * z
        case 3 => N3_3 * (xx - 3 * yy) * x
      }
      case 4 => m match {
        case -4 => N4_4 * 4 * x * y * (xx - yy)
        case -3 => N4_3 * (3 * xx - yy) * y * z
        case -2 => N4_2 * 2 * x * y * (7 * zz - 1.0)
        case -1 => N4_1 * y * z * (7 * zz - 3.0)
        case 0 => N4_0 * (35 * zz * zz - 30 * zz + 3.0)
        case 1 => N4_1 * x * z * (7 * zz - 3.0)
        case 2 => N4_2 * (xx - yy) * (7 * zz - 1.0)
        case 3 => N4_3 * x * z * (xx - 3 * yy)
        case 4 => N4_4 * (xx * (xx - 3 * yy) - yy * (3 * xx - yy))
      }
    }
  }
}
