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

package scalismo.faces.momo

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian

/** coefficients describing a Morphable Model instance */
case class MoMoCoefficients(shape: DenseVector[Double],
                            color: DenseVector[Double],
                            expression: DenseVector[Double]) {
  def *(f: Float): MoMoCoefficients = this * f.toDouble
  def *(d: Double): MoMoCoefficients = copy(shape = shape * d, color = color * d, expression = expression * d)
  def +(other: MoMoCoefficients): MoMoCoefficients = copy(shape = shape + other.shape, color = color + other.color, expression = expression + other.expression)
  def -(other: MoMoCoefficients): MoMoCoefficients = copy(shape = shape - other.shape, color = color - other.color, expression = expression - other.expression)
}

object MoMoCoefficients {
  def apply(shape: IndexedSeq[Double],
            color: IndexedSeq[Double],
            expression: IndexedSeq[Double]) = new MoMoCoefficients(DenseVector(shape.toArray), DenseVector(color.toArray), DenseVector(expression.toArray))

  def apply(shape: IndexedSeq[Double],
            color: IndexedSeq[Double]) = new MoMoCoefficients(DenseVector(shape.toArray), DenseVector(color.toArray), DenseVector.zeros[Double](0))

  def apply(shape: DenseVector[Double],
            color: DenseVector[Double]) = new MoMoCoefficients(shape, color, DenseVector.zeros[Double](0))

  /** get 0 coefficients of specified length */
  def zeros(shapeComponents: Int,
            colorComponents: Int,
            expressionComponents: Int): MoMoCoefficients = {
    new MoMoCoefficients(
      DenseVector.zeros(shapeComponents),
      DenseVector.zeros(colorComponents),
      DenseVector.zeros(expressionComponents))
  }
}
