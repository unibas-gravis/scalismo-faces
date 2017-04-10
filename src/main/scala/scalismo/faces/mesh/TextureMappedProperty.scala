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
import scalismo.faces.image.filter.ImageFilter
import scalismo.faces.image.{InterpolatedPixelImage, PixelImage, PixelImageDomain}
import scalismo.faces.render.{PixelShaders, PlainRenderBuffer, PointShader, TriangleRenderer}
import scalismo.geometry.Point._
import scalismo.geometry.{Point, _2D, _3D}
import scalismo.mesh.{BarycentricCoordinates, MeshSurfaceProperty, TriangleId, TriangleList}

import scala.reflect.ClassTag

/** texture mapping for a property: texture image contains property values, textureMapping maps surface to texture domain */
case class TextureMappedProperty[A](override val triangulation: TriangleList,
                                    textureMapping: MeshSurfaceProperty[Point[_2D]],
                                    texture: PixelImage[A])(implicit ops: ColorSpaceOperations[A])
  extends MeshSurfaceProperty[A] {
  val w: Int = texture.width
  val h: Int = texture.height

  val texInterpolated: InterpolatedPixelImage[A] = texture.interpolate

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

  /**
    * sample an arbitrary surface property into a texture-based property
    *
    * @param surfaceProperty surface property to sample
    * @param textureMapping texture coordinates used to sample the property
    * @param texImageDomain texture size
    * @param bgValue value of texture where no information is stored
    */
  def fromSurfaceProperty[Pixel: ClassTag](surfaceProperty: MeshSurfaceProperty[Pixel],
                                           textureMapping: MeshSurfaceProperty[Point[_2D]],
                                           texImageDomain: PixelImageDomain,
                                           bgValue: Pixel)(implicit ops: ColorSpaceOperations[Pixel]): TextureMappedProperty[Pixel] = {
    // triangulation
    val triangulation = surfaceProperty.triangulation

    // texture size
    val w = texImageDomain.width
    val h = texImageDomain.height

    // use rasterer: need shaders to render positions and surface property values
    // point shader is not needed to do anything
    val pointShader = new PointShader {
      override def apply(p: Point[_3D]): Point[_3D] = p
    }
    // accesses the surface property at each point of the triangle, the property itself
    val propertyShader = PixelShaders.PropertyShader(surfaceProperty)
    // texture image, invert buffer y inversion of renderer (we do this explicitly with UV coordinates here)
    val textureBuffer = PlainRenderBuffer(w, h, bgValue)
    def p3d(p: Point[_2D]) = Point(p.x, p.y, 0.0)
    // raster triangles
    for (tid <- triangulation.triangleIds) {
      // points in texture image
      val p1 = p3d(imageCoordinatesFromUV(textureMapping(tid, BarycentricCoordinates.v0), texImageDomain.width, texImageDomain.height))
      val p2 = p3d(imageCoordinatesFromUV(textureMapping(tid, BarycentricCoordinates.v1), texImageDomain.width, texImageDomain.height))
      val p3 = p3d(imageCoordinatesFromUV(textureMapping(tid, BarycentricCoordinates.v2), texImageDomain.width, texImageDomain.height))
      // raster triangle into texture image
      TriangleRenderer.rasterTriangle(tid, p1, p2, p3, pointShader, propertyShader, textureBuffer)
    }
    // create texture mapped property
    TextureMappedProperty(triangulation, textureMapping, textureBuffer.toImage)
  }
}
