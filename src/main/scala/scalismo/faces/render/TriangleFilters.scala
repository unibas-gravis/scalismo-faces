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

import scalismo.geometry.{Point, Vector, _3D}
import scalismo.mesh.{TriangleId, TriangleMesh}

/** methods to create useful triangle filters to use in TriangleRenderer, e.g. culling and clipping */
object TriangleFilters {
  /** remove triangles which face backwards, use as triangleFilter in renderMesh */
  def backfaceCullingFilter(worldMesh: TriangleMesh[_3D], eyePosition: Point[_3D]): TriangleId => Boolean = {
    triangleId =>
      val triangle = worldMesh.triangulation.triangle(triangleId)
      val a = worldMesh.pointSet.point(triangle.ptId1)
      worldMesh.cellNormals(triangleId).dot(a - eyePosition) <= 0.0
  }

  /** remove triangles *partially* behind clipping plane, use as triangleFilter in renderMesh */
  def clippingFilter(worldMesh: TriangleMesh[_3D], point: Point[_3D], normal: Vector[_3D]): TriangleId => Boolean = {
    triangleId =>
      val triangle = worldMesh.triangulation.triangle(triangleId)
      val a = worldMesh.pointSet.point(triangle.ptId1)
      val b = worldMesh.pointSet.point(triangle.ptId2)
      val c = worldMesh.pointSet.point(triangle.ptId3)
      def inFront(pt: Point[_3D]): Boolean = (pt - point).dot(normal) >= 0.0
      inFront(a) && inFront(b) && inFront(c)
  }

  /** remove triangles *completely* behind clipping plane, use as triangleFilter in renderMesh */
  def completeClippingFilter(worldMesh: TriangleMesh[_3D], point: Point[_3D], normal: Vector[_3D]): TriangleId => Boolean = {
    triangleId =>
      val triangle = worldMesh.triangulation.triangle(triangleId)
      val a = worldMesh.pointSet.point(triangle.ptId1)
      val b = worldMesh.pointSet.point(triangle.ptId2)
      val c = worldMesh.pointSet.point(triangle.ptId3)
      def inFront(pt: Point[_3D]): Boolean = (pt - point).dot(normal) >= 0.0
      inFront(a) || inFront(b) || inFront(c)
  }
}
