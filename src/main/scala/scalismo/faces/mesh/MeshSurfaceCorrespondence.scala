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

/**
  * express correspondence of a triangulated surface to another triangulation, maps points on this surface to target surface
  */
trait MeshSurfaceCorrespondence extends MeshSurfaceProperty[(TriangleId, BarycentricCoordinates)] {

  /**
    * get corresponding point on target surface
    * @param triangleId triangle on this surface
    * @param bcc barycentric coordinates on this surface
    * @return corresponding triangle and barycentric coordinates on target surface
    */
  def correspondingPoint(triangleId: TriangleId, bcc: BarycentricCoordinates): (TriangleId, BarycentricCoordinates)

  /**
    * triangulation of this surface
    * @return
    */
  override def triangulation: TriangleList

  /**
    * triangulation of target surface
    * @return
    */
  def targetTriangulation: TriangleList

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): (TriangleId, BarycentricCoordinates) = correspondingPoint(triangleId: TriangleId, bcc: BarycentricCoordinates)
}

/**
  * invertible surface correspondence
  */
trait Invertible { self: MeshSurfaceCorrespondence =>
  /**
    * inverse correspondence, mapping target surface to this surface
    */
  def inverse: MeshSurfaceCorrespondence
}

/**
  * use a target surface property through a surface correspondence
  * Each lookup on this surface returns the property at the corresponding point of the underlying surface
  * @param underlyingProperty surface property on target surface (underlying)
  * @param correspondence surface correspondence field mapping points in this triangulation to underlying triangulation
  * @tparam A type of property
  */
case class WarpedMeshSurfaceProperty[A](underlyingProperty: MeshSurfaceProperty[A], correspondence: MeshSurfaceCorrespondence) extends MeshSurfaceProperty[A] {
  require(underlyingProperty.triangulation == correspondence.targetTriangulation, "correspondence is not compatible with underlying property")

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    val oldSurfacePoint = correspondence.onSurface(triangleId, bcc)
    underlyingProperty(oldSurfacePoint._1, oldSurfacePoint._2)
  }

  override def triangulation: TriangleList = correspondence.triangulation
}