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

import scalismo.mesh.MeshCompactifier

/** Mask a MoMo according to supplied mask in the form of a MeshCompactifier.*/
case class MoMoMasker(op: MeshCompactifier) {
  val maskedMesh = op.transformedMesh
  val newIdx = maskedMesh.triangulation.pointIds
  val oldIds = newIdx.map(op.pointBackMap)

  def maskMoMo(model: MoMo): MoMo = {
    model match {
      case model: MoMoBasic => maskMoMoBasic(model)
      case model: MoMoExpress => maskMoMoExpress(model)
    }
  }

  def maskMoMoBasic(model: MoMoBasic): MoMoBasic = {
    val maskedModelShape = model.shape.marginal(oldIds)
    val maskedModelColor = model.color.marginal(oldIds)
    MoMo(maskedMesh, maskedModelShape, maskedModelColor)
  }

  def maskMoMoExpress(model: MoMoExpress): MoMoExpress = {
    val maskedModelShape = model.shape.marginal(oldIds)
    val maskedModelColor = model.color.marginal(oldIds)
    val maskedModelExpressions = model.expression.marginal(oldIds)
    MoMo(maskedMesh, maskedModelShape, maskedModelColor, maskedModelExpressions, model.landmarks)
  }
}