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

import scalismo.color.RGBA
import scalismo.faces.render.Transform3D
import scalismo.geometry.{EuclideanVector, _3D}
import scalismo.mesh._

/**
  * colored mesh
  * @param shape positions
  * @param color color of mesh surface
  */
case class ColorMesh3D(shape: TriangleMesh3D, color: MeshSurfaceProperty[RGBA]) {
  require(shape.triangulation == color.triangulation)

  def transform(trafo: Transform3D): ColorMesh3D = {
    val s = shape.transform{trafo(_)}
    copy(shape = s)
  }
}


/**
  * colored mesh with normals
  * @param shape meshed surface, positions
  * @param color color of mesh surface
  * @param normals normal of mesh surface
  */
case class ColorNormalMesh3D(shape: TriangleMesh3D, color: MeshSurfaceProperty[RGBA], normals: MeshSurfaceProperty[EuclideanVector[_3D]]) {
  // @todo wait for scalismo to fix issue #138
  //  require(shape.triangulation == color.triangulation)
  //  require(shape.triangulation == normals.triangulation)

  def transform(trafo: Transform3D): ColorNormalMesh3D = {
    val n = normals.map{trafo(_).normalize}
    val s = shape.transform{trafo(_)}
    copy(shape = s, normals = n)
  }

  def withVertexNormals: ColorNormalMesh3D = copy(normals = shape.vertexNormals)

  def withFaceNormals: ColorNormalMesh3D = copy(normals = shape.cellNormals)
}

object ColorNormalMesh3D {
  def apply(vcMesh: VertexColorMesh3D): ColorNormalMesh3D = ColorNormalMesh3D(vcMesh.shape, vcMesh.color, vcMesh.shape.vertexNormals)
}

/**
  * mesh with texture color
  * @param shape underlying surface mesh
  * @param texture color on surface, represented with a texture */
case class TexturedMesh3D(shape: TriangleMesh3D, texture: TextureMappedProperty[RGBA])

/**
  * flexible mesh container with optional color and normals
  * @param shape surface of mesh
  * @param color optional surface color
  * @param normals optional surface normals
  * */
case class OptionalColorNormalMesh3D(shape: TriangleMesh3D,
                                     color: Option[MeshSurfaceProperty[RGBA]],
                                     normals: Option[MeshSurfaceProperty[EuclideanVector[_3D]]]) {

  def hasColor: Boolean = color.isDefined

  def hasNormals: Boolean = normals.isDefined

  def hasTexture: Boolean = color.exists{col => col.isInstanceOf[TextureMappedProperty[_]]}

  def hasVertexColor: Boolean = hasColor && !hasTexture

  /** this mesh as vertex color mesh, only if underlying mesh is a vertex color mesh (to convert use toVertexColorMesh3D) */
  def vertexColorMesh3D: Option[VertexColorMesh3D] = {
    val vertexColor = color.collect{ case col: SurfacePointProperty[RGBA] => col}
    vertexColor.map{col => VertexColorMesh3D(shape, col)}
  }

  /** convert to vertex color mesh by sampling values at vertex points or using existing vertex color */
  def toVertexColorMesh3D: Option[VertexColorMesh3D] = {
    color.map {
      case pointColor: SurfacePointProperty[RGBA] => VertexColorMesh3D(shape, pointColor)
      case color: MeshSurfaceProperty[RGBA] => VertexColorMesh3D(shape, SurfacePointProperty.averagedPointProperty(color))
    }
  }

  /** get a mesh with color and normals (if supported by underlying loaded mesh) */
  def colorNormalMesh3D: Option[ColorNormalMesh3D] = {
    for {
      col <- color
      norm <- normals
    } yield ColorNormalMesh3D(shape, col, norm)
  }

  /** get a texture-colored mesh (if supported by underlying loaded mesh) */
  def texturedMesh3D: Option[TexturedMesh3D] = {
    val texture = color.collect{case tc: TextureMappedProperty[RGBA] => tc}
    for {
      tex <- texture
    } yield TexturedMesh3D(shape, tex)
  }

  def transform(trafo: Transform3D): OptionalColorNormalMesh3D = {
    val s = shape.transform{trafo(_)}
    val n = for(norm <- normals) yield norm.map{trafo(_)}
    copy(shape = s, normals = n)
  }
}

object OptionalColorNormalMesh3D {
  def fromVertexColorMesh(vcMesh: VertexColorMesh3D): OptionalColorNormalMesh3D = OptionalColorNormalMesh3D(vcMesh.shape, Some(vcMesh.color), None)
  def fromTriangleMesh(triangleMesh3D: TriangleMesh3D): OptionalColorNormalMesh3D = OptionalColorNormalMesh3D(triangleMesh3D, None, None)
  def fromColorNormalMesh(cnMesh: ColorNormalMesh3D): OptionalColorNormalMesh3D = OptionalColorNormalMesh3D(cnMesh.shape, Some(cnMesh.color), Some(cnMesh.normals))
}