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

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.render.TriangleRenderer.TriangleFragment
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.{BarycentricCoordinates, TriangleId}

/** transform a point from 3D space to normalized device coordinates: [-1,1]x[-1,1]x[-1,1] (normalized to viewing frustum) */
trait PointShader extends (Point[_3D] => Point[_3D]) {
  def bccScreenToWorld(screenBCC: BarycentricCoordinates, a: Point[_3D], b: Point[_3D], c: Point[_3D]): BarycentricCoordinates = screenBCC
  def bccWorldToScreen(worldBCC: BarycentricCoordinates, a: Point[_3D], b: Point[_3D], c: Point[_3D]): BarycentricCoordinates = worldBCC
}

object PointShader {
  val identity = new PointShader {
    override def apply(p: Point[_3D]): Point[_3D] = p
  }
}

/** paint a pixel during rendering */
trait PixelShader[A] { self =>
  /**
   * paint the pixel, has access to
   *
   * @param triangleId Id of the current triangle
   * @param worldBCC Barycentric coordinates of the pixel within the triangle (in *world* space -- corrected)
   * @param screenCoordinates Coordinates of the pixel on the screen (x, y: image coordinates, z: depth value)
   */
  def apply(triangleId: TriangleId,
    worldBCC: BarycentricCoordinates,
    screenCoordinates: Point[_3D]): A

  /**
   * paint the pixel, has access to
   *
   * @param triangleId Id of the current triangle
   * @param worldBCC Barycentric coordinates of the pixel within the triangle (in *world* space -- corrected)
   * @param screenCoordinates Coordinates of the pixel on the screen (x, y: image coordinates, z: depth value)
   * @param ccwWinding true if the triangle has counter clock-wise winding (default)
   */
  def apply(triangleId: TriangleId,
    worldBCC: BarycentricCoordinates,
    screenCoordinates: Point[_3D],
    ccwWinding: Boolean): A = this(triangleId, worldBCC, screenCoordinates)

  def apply(fragment: TriangleFragment): A = this(fragment.triangleId, fragment.worldBCC, Point(fragment.x, fragment.y, fragment.z))

  def +(other: PixelShader[A])(implicit ops: ColorSpaceOperations[A]) = new PixelShader[A] {
    import ColorSpaceOperations.implicits._
    override def apply(triangleId: TriangleId,
      worldBCC: BarycentricCoordinates,
      screenCoordinates: Point[_3D]): A = self(triangleId, worldBCC, screenCoordinates) + other(triangleId, worldBCC, screenCoordinates)
  }

  def *(other: PixelShader[A])(implicit ops: ColorSpaceOperations[A]) = new PixelShader[A] {
    import ColorSpaceOperations.implicits._
    override def apply(triangleId: TriangleId,
      worldBCC: BarycentricCoordinates,
      screenCoordinates: Point[_3D]): A = self(triangleId, worldBCC, screenCoordinates) multiply other(triangleId, worldBCC, screenCoordinates)
  }

  def map[B](f: A => B) = new PixelShader[B] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): B = f(self(triangleId, worldBCC, screenCoordinates))
  }

}

object PixelShader {
  def apply[A](f: (TriangleId, BarycentricCoordinates) => A) = new PixelShader[A] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): A = f(triangleId, worldBCC)
  }

  def apply[A](f: (TriangleId, BarycentricCoordinates, Point[_3D]) => A) = new PixelShader[A] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): A = f(triangleId, worldBCC, screenCoordinates)
  }
}
