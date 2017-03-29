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

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.PixelImage
import scalismo.faces.image.filter.ImageFilter
import scalismo.geometry.Point._
import scalismo.geometry.{Point, _2D}
import scalismo.mesh.{BarycentricCoordinates, MeshSurfaceProperty, TriangleId, TriangleList}

/** texture mapping for a property: texture image contains property values, textureMapping maps surface to texture domain */
case class TextureMappedProperty[A](override val triangulation: TriangleList,
                                    textureMapping: MeshSurfaceProperty[Point[_2D]],
                                    texture: PixelImage[A])(implicit ops: ColorSpaceOperations[A])
  extends MeshSurfaceProperty[A] {
  val w = texture.width
  val h = texture.height

  val texInterpolated = texture.interpolate

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    val t = triangulation.triangle(triangleId)
    // get texture coordinates at current location
    val imgCoords = TextureMappedProperty.imageInterpolatedCoordinatesFromUV(textureMapping(triangleId, bcc), w, h)
    // texture value at point
    texInterpolated(imgCoords.x, imgCoords.y)
  }

  def map[B](f: PixelImage[A] => PixelImage[B])(implicit ops: ColorSpaceOperations[B]): TextureMappedProperty[B] = {
    TextureMappedProperty(triangulation, textureMapping, texture.filter(ImageFilter(f)))
  }
}

object TextureMappedProperty {
  @inline
  def imageInterpolatedCoordinatesFromUV(texCoord: Point[_2D], w: Int, h: Int) = Point(texCoord.x * w, (1 - texCoord.y) * h)
  @inline
  def imageCoordinatesFromUV(texCoord: Point[_2D], w: Int, h: Int) = Point(texCoord.x * w - 0.5, (1 - texCoord.y) * h - 0.5)
  @inline
  def imageCoordinatesToUV(imageCoord: Point[_2D], w: Int, h: Int) = Point((imageCoord.x + 0.5) / w, 1 - (imageCoord.y + 0.5) / h)
}
