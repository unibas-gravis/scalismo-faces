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

package scalismo.faces.sampling.face

import breeze.linalg.DenseVector
import scalismo.faces.mesh.VertexColorMesh3D
import scalismo.faces.momo.MoMo
import scalismo.faces.parameters.{MoMoInstance, RenderParameter}

/** generates a model instance in original model coordinates */
class ParametricModel(model: MoMo ) {
  /** pad a coefficient vector if it is too short, basis with single vector */
  private def padCoefficients(coefficients: DenseVector[Double], rank: Int): DenseVector[Double] = {
    require(coefficients.length <= rank, "too many coefficients for model")
    if (coefficients.length == rank)
      coefficients
    else
      DenseVector(coefficients.toArray ++ Array.fill(rank - coefficients.length)(0.0))
  }

  /** create an instance of the model, in the original model's object coordinates */
  def instance(parameters: RenderParameter): VertexColorMesh3D = {
    instanceFromCoefficients(parameters.momo)
  }

  /** draw a model instance directly from the coefficients */
  def instanceFromCoefficients(instance: MoMoInstance): VertexColorMesh3D = {
    model.instance(instance.coefficients)
  }
}
