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

package scalismo.faces.io.msh

import java.io._

import scalismo.faces.color.RGBA
import scalismo.faces.mesh._
import scalismo.geometry._
import scalismo.mesh._

import scala.util.Try

/** provides methods to write and read mesh files in Gravis MSH mesh format (rather use PLY as exchangeable mesh format) */
object MSHMeshIO {

  def read(file: File): Try[MSHMesh] = Try {
    val filename = file.getAbsolutePath
    GravisMSHFormat.Reader.readMSHMesh(file)
  }

  def write(mesh: MSHMesh, file: File): Try[Unit] = Try {
    val filename = file.getAbsolutePath
    GravisMSHFormat.Writer.writeMSHMesh(mesh, file)
  }

  def write(mesh: TriangleMesh3D, color: Option[MeshSurfaceProperty[RGBA]], normals: Option[MeshSurfaceProperty[Vector[_3D]]], file: File): Try[Unit] = {
    // create a MSH mesh from provided data
    val mshMesh = MSHMesh.fromTriangleMesh3D(mesh, color, normals)
    write(mshMesh, file)
  }

  def write(mesh: VertexColorMesh3D, file: File): Try[Unit] = {
    val mshMesh = MSHMesh.fromTriangleMesh3D(mesh.shape, Option(mesh.color), Option(mesh.shape.vertexNormals))
    write(mshMesh, file)
  }
}
