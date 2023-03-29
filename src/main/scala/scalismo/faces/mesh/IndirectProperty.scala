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

package scalismo.faces.mesh

import scalismo.mesh.{BarycentricCoordinates, MeshSurfaceProperty, TriangleId, TriangleList}

/** indirected property, can be used to attach different materials */
case class IndirectProperty[@specialized(Double, Float, Int, Boolean) A](override val triangulation: TriangleList,
                                                                         triangleIndirectionIndex: IndexedSeq[Int],
                                                                         properties: IndexedSeq[MeshSurfaceProperty[A]]
) extends MeshSurfaceProperty[A] {
  require(triangleIndirectionIndex.size == triangulation.triangleIds.size,
          "IndirectedProperty: Indirection index is not per triangle."
  )
  require(triangleIndirectionIndex.forall(properties.isDefinedAt))

  /** access via triangle coordinates */
  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    // get real surface property
    val property = properties(triangleIndirectionIndex(triangleId.id))
    property(triangleId, bcc)
  }
}
