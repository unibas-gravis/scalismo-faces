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

import scalismo.faces.FacesTestSuite
import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.image.InterpolationKernel.BilinearKernel
import scalismo.faces.image.PixelImage
import scalismo.faces.mesh.{ColorNormalMesh3D, TextureMappedProperty}
import scalismo.faces.parameters.{Camera, Pose, RenderParameter}
import scalismo.faces.render.PixelShaders.PropertyShader
import scalismo.geometry.Vector
import scalismo.mesh.{MeshSurfaceProperty, SurfacePointProperty, TriangleMesh3D}

class TextureExtractionTests extends FacesTestSuite {
  import scalismo.faces.image.PixelImage.implicits._

  val w = 100
  val h = 100

  val texW = 123
  val texH = 91

  val gridMesh: ColorNormalMesh3D = randomGridMeshWithTexture(200, 200)
  val mesh: TriangleMesh3D = gridMesh.shape
  val vertexColor: SurfacePointProperty[RGBA] = SurfacePointProperty.averagedPointProperty(gridMesh.color)
  val texture: TextureMappedProperty[RGBA] = gridMesh.color.asInstanceOf[TextureMappedProperty[RGBA]]

  val texImage: PixelImage[RGBA] = texture.texture
  val rndTexture: PixelImage[RGBA] = PixelImage(texImage.width / 20, texImage.height / 20, (x, y) => randomRGBA).resample(texImage.width, texImage.height, BilinearKernel)

  val rps: RenderParameter = RenderParameter.default.copy(
    camera = Camera.for35mmFilm(focalLength = 100),
    pose = Pose.neutral.copy(translation = Vector(-100, -100, -1000))).noLightAndColor

  val renderedImage: PixelImage[RGBA] = TriangleRenderer.renderMesh(mesh, rps.pointShader, PropertyShader(texture), ZBuffer[RGBA](rps.imageSize.width, rps.imageSize.height, RGBA(1, 0, 1, 0))).toImage

  val frustum = Frustum(-1, 1, -1, 1, 0.1, 2)
  val perspP = FrustumPinholeProjection(frustum)
  val pointShaderPP: PointShader = perspP.pointShader(Transform3D.identity)
  val orthoP = FrustumOrthographicProjection(frustum)
  val pointShaderOP: PointShader = orthoP.pointShader(Transform3D.identity)

  def imDiffRGB(img1: PixelImage[RGB], img2: PixelImage[RGB]): Double = (img1 - img2).norm

  def imDiffRGBA(img1: PixelImage[RGBA], img2: PixelImage[RGBA]): Double = {
    val diff = PixelImage(img1.width, img1.height, (x, y) => (img1(x, y).a * img2(x, y).a) *: (img1(x, y) - img2(x, y)))
    diff.norm
  }

  describe("TextureExtractionTests") {
    // make random image
    val rndImage: PixelImage[RGBA] = randomImage(rps.imageSize.width / 10, rps.imageSize.height / 10).resample(rps.imageSize.width, rps.imageSize.height).mapLazy(_.toRGBA)

    // imageAsSurfaceProperty
    it("should extract and re-render an image as surface property consistently") {
      // extract texture as surface property
      val imageAsSurfaceProp: MeshSurfaceProperty[RGBA] = TextureExtraction.imageAsSurfaceProperty(mesh, rps.pointShader, rndImage).map(_.getOrElse(RGBA(1, 1, 0, 0)))
      // render extracted texture as surface property
      val rendImageAsProperty = TriangleRenderer.renderMesh(mesh, rps.pointShader, PropertyShader(imageAsSurfaceProp), rps.imageSize.zBuffer(RGBA.BlackTransparent)).toImage
      // images should match
      imDiffRGBA(rendImageAsProperty, rndImage) should be < 1.0
    }

    // imageAsTexture
    it("should use the image as a texture using the rendered points as texture mapping and re-render consistently") {
      // extract texture
      val imageAsTexture: TextureMappedProperty[RGBA] = TextureExtraction.imageAsTexture(mesh, rps.pointShader, rndImage)
      // render with given texture
      val rendImageAsTexture = TriangleRenderer.renderMesh(mesh, rps.pointShader, PropertyShader(imageAsTexture), rps.imageSize.zBuffer(RGBA.BlackTransparent)).toImage
      // images should match
      imDiffRGBA(rendImageAsTexture, rndImage) should be < 1.0
    }

    // sample TextureMappedProperty from imageAsSurface property
    it("should extract an image as surface property, raster it into a texture map representation and re-render it consistently") {
      // extract texture as surface property
      val imageAsSurfaceProp: MeshSurfaceProperty[RGBA] = TextureExtraction.imageAsSurfaceProperty(mesh, rps.pointShader, rndImage).map(_.getOrElse(RGBA(1, 1, 0, 1)))
      // texture map representation
      val tex = TextureMappedProperty.fromSurfaceProperty(imageAsSurfaceProp, texture.textureMapping, texture.texture.domain, RGBA(0, 1, 0, 1))
      // render extracted texture as surface property
      val rendImageTex = TriangleRenderer.renderMesh(mesh, rps.pointShader, PropertyShader(tex), rps.imageSize.zBuffer(RGBA.BlackTransparent)).toImage
      // images should match
      imDiffRGBA(rendImageTex, rndImage) should be < 32.0
    }

    // sample TextureMappedProperty from imageAsSurface property
    it("should extract an image as texture, raster it into a texture map representation and re-render it consistently") {
      // extract texture as surface property
      val imageAsTexture: MeshSurfaceProperty[RGBA] = TextureExtraction.imageAsTexture(mesh, rps.pointShader, rndImage)
      // texture map representation
      val tex: TextureMappedProperty[RGBA] = TextureMappedProperty.fromSurfaceProperty(imageAsTexture, texture.textureMapping, texture.texture.domain, RGBA(0, 1, 0, 1))
      // render extracted texture as surface property
      val rendImageTex = TriangleRenderer.renderMesh(mesh, rps.pointShader, PropertyShader(tex), rps.imageSize.zBuffer(RGBA.BlackTransparent)).toImage
      // images should match
      imDiffRGBA(rendImageTex, rndImage) should be < 30.0
    }

    // extractTextureAsImage
    it("should extract and re-render an image into a texture map representation consistently") {
      // extract texture image
      val texImg = TextureExtraction.extractTextureAsImage(mesh, rps.pointShader, rndImage, texture.texture.domain, texture.textureMapping).map(_.getOrElse(RGBA(1, 1, 0, 1)))
      // render extracted texture
      val tex = texture.copy(texture = texImg)
      val rendImageTexEx = TriangleRenderer.renderMesh(mesh, rps.pointShader, PropertyShader(tex), rps.imageSize.zBuffer(RGBA.BlackTransparent)).toImage
      // images should match
      imDiffRGBA(rendImageTexEx, rndImage) should be < 32.0
    }
  }
}
