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

import scalismo.geometry.{_3D, IntVector}
import scalismo.mesh._
import scalismo.numerics.ValueInterpolator

import scala.reflect.ClassTag

/**
 * surface property which is parameterized by a triangle-based indirection: vertex values per triangle, see old Gravis
 * mesh format
 */
case class VertexPropertyPerTriangle[A](override val triangulation: TriangleList,
                                        triangleVertexIndex: IndexedSeq[IntVector[_3D]],
                                        vertexData: IndexedSeq[A]
)(implicit interpolator: ValueInterpolator[A])
    extends MeshSurfaceProperty[A] {

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    val ind = triangleVertexIndex(triangleId.id)
    bcc.interpolateProperty(vertexData(ind(0)), vertexData(ind(1)), vertexData(ind(2)))
  }

  @deprecated("should be replaced by triangleVertexIndex", "0.5")
  def triangleIndexToIndexedSeq = triangleVertexIndex

  @deprecated("should be replaced by vertexData.toArray", "0.5")
  def dataToArray(implicit tag: ClassTag[A]) = vertexData.toArray

  @deprecated("should be replaced by vertexData", "0.5")
  def dataToIndexedSeq = vertexData

  private lazy val collectDataRange: Int = {
    (triangleVertexIndex.map(_.i) ++ triangleVertexIndex.map(_.j) ++ triangleVertexIndex.map(_.k)).max + 1
  }
}

object VertexPropertyPerTriangle {
  def fromPointProperty[A](property: SurfacePointProperty[A])(implicit interpolator: ValueInterpolator[A]) = {
    // point property: just rebuild a triangle-data-index
    val trIndex = property.triangulation.triangles.map(t => IntVector(t.ptId1.id, t.ptId2.id, t.ptId3.id))
    VertexPropertyPerTriangle(property.triangulation, trIndex, property.pointData)
  }

  def fromTriangleProperty[A](property: TriangleProperty[A])(implicit interpolator: ValueInterpolator[A]) = {
    val trIndex = property.triangulation.triangleIds.map(t => IntVector(t.id, t.id, t.id))
    VertexPropertyPerTriangle(property.triangulation, trIndex, property.triangleData)
  }

  def sampleSurfaceProperty[A](property: MeshSurfaceProperty[A])(implicit interpolator: ValueInterpolator[A]) = {
    val triangulation = property.triangulation
    val n = triangulation.triangleIds.size
    val data = new collection.mutable.ArrayBuffer[A](3 * n)
    for (t <- triangulation.triangleIds) {
      data += property(t, BarycentricCoordinates.v0)
      data += property(t, BarycentricCoordinates.v1)
      data += property(t, BarycentricCoordinates.v2)
    }
    val trIndex: IndexedSeq[IntVector[_3D]] =
      (0 until 3 * n).grouped(3).map(g => IntVector(g(0), g(1), g(2))).toIndexedSeq
    VertexPropertyPerTriangle(triangulation, trIndex, data.toIndexedSeq)
  }

  def fromSurfaceProperty[A](property: MeshSurfaceProperty[A])(implicit interpolator: ValueInterpolator[A]) =
    property match {
      case pp: SurfacePointProperty[A]       => fromPointProperty(pp)
      case tp: TriangleProperty[A]           => fromTriangleProperty(tp)
      case tvp: VertexPropertyPerTriangle[A] => tvp
      case _                                 => sampleSurfaceProperty(property)
    }
}
