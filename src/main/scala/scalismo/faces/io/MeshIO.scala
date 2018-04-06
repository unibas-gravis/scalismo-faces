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

package scalismo.faces.io

import java.io.{File, IOException}

import scalismo.faces.color.RGBA
import scalismo.faces.io.msh.MSHMeshIO
import scalismo.faces.io.ply.PLYMesh
import scalismo.faces.mesh.{ColorNormalMesh3D, OptionalColorNormalMesh3D, TextureMappedProperty, VertexColorMesh3D}
import scalismo.geometry.{Vector, _3D}
import scalismo.mesh.{MeshSurfaceProperty, SurfacePointProperty, TriangleMesh3D}

import scala.util.{Failure, Try}

/** provides general read and write functionality for various mesh formats with color information */
object MeshIO {

  /** general mesh reading in all supported file formats
    * read a mesh as flexible container type OptionalColorNormalMesh3D
    * if more detailed information is required use format-specific readers */
  def read(file: File): Try[OptionalColorNormalMesh3D] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.toLowerCase.endsWith(".msh.gz") || f.toLowerCase.endsWith(".msh") =>
        for (msh <- MSHMeshIO.read(file)) yield
          OptionalColorNormalMesh3D(msh.triangleMesh, msh.getColor, msh.getNormal)
      case f if f.toLowerCase.endsWith(".ply") =>
        PLYMesh.readColorNormalMesh3D(filename).map(OptionalColorNormalMesh3D.fromColorNormalMesh)
          .orElse(
            PLYMesh.readVertexColorMesh3D(filename).map(OptionalColorNormalMesh3D.fromVertexColorMesh))
          .orElse(
            PLYMesh.readTriangleMesh3D(filename).map(OptionalColorNormalMesh3D.fromTriangleMesh))
      case _ => Failure(new IOException("Reading mesh: Unknown file type " + filename))
    }
  }

  /** write a mesh, optionally with color and normals, automatically chooses format based on filename
    * supports only basic color and normal types, for more control use format-specific writers */
  def write(mesh: OptionalColorNormalMesh3D, file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.toLowerCase.endsWith(".msh.gz") || f.toLowerCase.endsWith(".msh") =>
        MSHMeshIO.write(mesh.shape, mesh.color, mesh.normals, file)
      case f if f.toLowerCase.endsWith(".ply") =>
        if (mesh.colorNormalMesh3D.isDefined)
          Try(PLYMesh.writePLY(mesh.colorNormalMesh3D.get, filename))
        else if (mesh.hasColor)
          mesh.color match {
            case texture: TextureMappedProperty[RGBA] =>
              Try(PLYMesh.writePLY(mesh.copy(normals = Some(mesh.shape.vertexNormals)).colorNormalMesh3D.get, filename))
            case vertexColor: SurfacePointProperty[RGBA] =>
              Try(PLYMesh.writePLY(mesh.vertexColorMesh3D.get, filename))
            case _ =>
              Failure(new IOException("Do not know how to write the color information of the given mesh to the file."))
          }
        else
          Try(PLYMesh.writePLY(mesh.shape, filename))
      case _ => Failure(new IOException("Writing mesh: Unknown file type " + filename))
    }
  }

  /** write a mesh, optionally with color and normals, automatically chooses format based on filename */
  def write(mesh: TriangleMesh3D, color: Option[MeshSurfaceProperty[RGBA]], normals: Option[MeshSurfaceProperty[Vector[_3D]]], file: File): Try[Unit] = {
    write(OptionalColorNormalMesh3D(mesh, color, normals), file)
  }

  /** write a vertex color mesh, automatically chooses format based on filename */
  def write(mesh: VertexColorMesh3D, file: File): Try[Unit] = {
    write(OptionalColorNormalMesh3D.fromVertexColorMesh(mesh), file)
  }

  /** write a mesh with color and normal properties, automatically chooses format based on filename */
  def write(mesh: ColorNormalMesh3D, file: File): Try[Unit] = {
    write(OptionalColorNormalMesh3D.fromColorNormalMesh(mesh), file)
  }
}
