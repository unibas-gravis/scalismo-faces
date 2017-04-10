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

package scalismo.faces.parameters

import scalismo.faces.color.RGBA
import scalismo.faces.image.PixelImage
import scalismo.faces.mesh.{ColorNormalMesh3D, VertexColorMesh3D}
import scalismo.faces.render.{TriangleFilters, TriangleRenderer, ZBuffer}
import scalismo.mesh.{MeshSurfaceProperty, TriangleMesh3D}

import scala.reflect.ClassTag

object ParametricRenderer {
  /**
    * render a mesh with specified colors and normals according to scene description parameter
    *
    * @param parameter scene description
    * @param mesh mesh to render, has positions, colors and normals
    * @param clearColor background color of buffer
    * @return
    */
  def renderParameterMesh(parameter: RenderParameter,
                          mesh: ColorNormalMesh3D,
                          clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
    val buffer = ZBuffer(parameter.imageSize.width, parameter.imageSize.height, clearColor)

    val worldMesh = mesh.transform(parameter.modelViewTransform)
    val backfaceCullingFilter = TriangleFilters.backfaceCullingFilter(worldMesh.shape, parameter.view.eyePosition)

    TriangleRenderer.renderMesh(
      mesh.shape,
      backfaceCullingFilter,
      parameter.pointShader,
      parameter.imageSize.screenTransform,
      parameter.pixelShader(mesh),
      buffer).toImage
  }

  /**
    * render according to parameters, convenience for vertex color mesh with vertex normals
    *
    * @param parameter scene description
    * @param mesh mesh to render, vertex color, vertex normals
    * @param clearColor background color of buffer
    * @return
    */
  def renderParameterVertexColorMesh(parameter: RenderParameter,
                                     mesh: VertexColorMesh3D,
                                     clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
    renderParameterMesh(parameter, ColorNormalMesh3D(mesh), clearColor)
  }

  /**
    * render the object described by the render parameters
    *
    * @param parameter scene description, including the object
    * @param clearColor background color of scene/buffer
    * @return
    */
  def renderParameter(parameter: RenderParameter,
                      clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
    val mesh = RenderObject.instance(parameter.momo)
    renderParameterMesh(parameter, mesh, clearColor)
  }

  /**
    * render an arbitrary property on the mesh into buffer (rasterization)
    *
    * @param renderParameter scene description
    * @param mesh mesh to render, positions
    * @param property surface property to rasterize
    * @tparam A type of surface property
    * @return
    */
  def renderPropertyImage[A: ClassTag](renderParameter: RenderParameter,
                                       mesh: TriangleMesh3D,
                                       property: MeshSurfaceProperty[A]): PixelImage[Option[A]] = {
    TriangleRenderer.renderPropertyImage(mesh,
      renderParameter.pointShader,
      property,
      renderParameter.imageSize.width,
      renderParameter.imageSize.height
    )
  }
}