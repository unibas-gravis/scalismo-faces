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

import scalismo.color.ColorSpaceOperations
import scalismo.faces.image.{InterpolatedPixelImage, PixelImage, PixelImageDomain}
import scalismo.faces.mesh.TextureMappedProperty
import scalismo.geometry.{Point, _2D, _3D}
import scalismo.mesh._

import scala.util.Try

/** methods to extract texture from images when rendering a mesh */
object TextureExtraction {
  /**
    * Texture Extraction from Image.
    * 1. Find correspondence between mesh and target image
    * Render Points on image plane according to pointShader
    * Put the 2D coordinates of the image onto the mesh. This is the texture mapping.
    */
  def imageAsSurfaceProperty[Pixel](mesh: TriangleMesh3D,
                                    pointShader: PointShader,
                                    targetImage: PixelImage[Pixel])(implicit ops: ColorSpaceOperations[Pixel]): MeshSurfaceProperty[Option[Pixel]] = {
    val visible: MeshSurfaceProperty[Boolean] = TriangleRenderer.visibilityAsSurfaceProperty(mesh, pointShader, targetImage.domain, 1e-3, boundaryAlwaysVisible = false)

    new MeshSurfaceProperty[Option[Pixel]] {
      val target: InterpolatedPixelImage[Pixel] = targetImage.interpolate

      override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): Option[Pixel] = {
        val vis: Boolean = visible(triangleId, bcc)
        if (vis) {
          val imagePoint: Point[_3D] = TriangleRenderer.transformPoint(mesh.position(triangleId, bcc), pointShader, targetImage.domain)
          Try(target(imagePoint.x + 0.5, imagePoint.y + 0.5)).toOption // interpolated image access is shifted by 0.5/0.5
        } else
          None
      }

      override def triangulation: TriangleList = mesh.triangulation
    }
  }

  /**
    * Texture Extraction from Image.
    * 1. Find correspondence between mesh and target image.
    * Render Points on image plane according to pointShader.
    * Put the 2D coordinates of the image onto the mesh. This is the texture mapping.
    */
  def imageAsTexture[Pixel](mesh: TriangleMesh3D,
                            pointShader: PointShader,
                            targetImage: PixelImage[Pixel])(implicit ops: ColorSpaceOperations[Pixel]): TextureMappedProperty[Pixel] = {

    // get canonical mapping: projected points in image (image itself becomes texture)
    val projectedPoints: IndexedSeq[Point[_2D]] = mesh.pointSet.points.map(p => {
      val p2d = TriangleRenderer.transformPoint(p, pointShader, targetImage.domain)
      Point(p2d.x, p2d.y)
    }).toIndexedSeq

    val projectedUVPoints = projectedPoints.map(p => TextureMappedProperty.imageCoordinatesToUV(p, targetImage.width, targetImage.height))
    val imageTextureCoordinates: SurfacePointProperty[Point[_2D]] = SurfacePointProperty(mesh.triangulation, projectedUVPoints)
    TextureMappedProperty(mesh.triangulation, imageTextureCoordinates, targetImage)
  }

  /** full texture extraction: texture image extracted through a mesh rendered to an image, image "pulled back" from image to texture domain using texture mapping */
  def extractTextureAsImage[A](mesh: TriangleMesh3D,
                               pointShader: PointShader,
                               targetImage: PixelImage[A],
                               textureDomain: PixelImageDomain,
                               textureMap: MeshSurfaceProperty[Point[_2D]])(implicit ops: ColorSpaceOperations[A]): PixelImage[Option[A]] = {
    val imgOnSurface = imageAsSurfaceProperty(mesh, pointShader, targetImage)
    val texturedSurface: TextureMappedProperty[Option[A]] = TextureMappedProperty.fromSurfaceProperty(imgOnSurface, textureMap, textureDomain, None)
    texturedSurface.texture
  }
}
