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
package scalismo.faces.io.ply

import java.io._
import java.nio.ByteOrder
import java.util.Scanner

import scalismo.color.RGBA
import scalismo.faces.image.PixelImage
import scalismo.faces.mesh.{TextureMappedProperty, VertexPropertyPerTriangle}
import scalismo.geometry._
import scalismo.mesh._

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal


/**
  * Function that try to parse certain mesh properties from the data read of a ply file by the PlyReader.
  */
object PlyMeshReader {

  import PlyHelpers._
  import PlyMeshHelper._

  def getProperties(values: List[(String, List[(String, List[_])])]): (List[(String, List[_])], List[(String, List[_])]) = {
    (getVetexProperties(values), getFaceProperties(values))
  }

  def getFaceProperties(values: List[(String, List[(String, List[_])])]): List[(String, List[_])] = {
    values.find(e => e._1 == PLY.face).getOrElse(
      throw new IOException("Could not read face properties.")
    )._2
  }

  def getVetexProperties(values: List[(String, List[(String, List[_])])]): List[(String, List[_])] = {
    values.find(e => e._1 == PLY.vertex).getOrElse(
      throw new IOException("Could not read vertex properties.")
    )._2
  }

  def getVertices(values: List[(String, List[_])]): IndexedSeq[Point[_3D]] = {
    val x = values.find(e => e._1 == PLY.xCoordinate).getOrElse(
      throw new IOException("Could not read x coordinates.")
    )._2.map(x => any2Double(x))

    val y = values.find(e => e._1 == PLY.yCoordinate).getOrElse(
      throw new IOException("Could not read y coordinates.")
    )._2.map(x => any2Double(x))

    val z = values.find(e => e._1 == PLY.zCoordinate).getOrElse(
      throw new IOException("Could not read z coordinates.")
    )._2.map(x => any2Double(x))

    (x, y, z).zipped.toIndexedSeq.map(t => Point(t._1, t._2, t._3))
  }

  def getTriangles(values: List[(String, List[_])]): TriangleList = {
    val indices = values.find(e => e._1 == PLY.vertexIndices).getOrElse(
      throw new IOException("Could not read vertex indices.")
    )._2.grouped(3).map(a => listOfAny2ListOfPointIDs(a))
    TriangleList(indices.map(l => TriangleCell(l(0), l(1), l(2))).toIndexedSeq)
  }


  def getColors(values: List[(String, List[_])]): IndexedSeq[RGBA] = {
    val reds: Seq[Double] = values.find(e => e._1 == PLY.red).getOrElse(
      throw new IOException("Could not read red color channel.")
    )._2.map(x => byte2ZeroOne(any2Byte(x)))

    val greens: Seq[Double] = values.find(e => e._1 == PLY.green).getOrElse(
      throw new IOException("Could not read green color channel.")
    )._2.map(x => byte2ZeroOne(any2Byte(x)))

    val blues: Seq[Double] = values.find(e => e._1 == PLY.blue).getOrElse(
      throw new IOException("Could not read blue color channel.")
    )._2.map(x => byte2ZeroOne(any2Byte(x)))

    val alphas: Option[Seq[Double]] = values.find(e => e._1 == PLY.alpha).map{
      _._2.map(x => byte2ZeroOne(any2Byte(x)))
    }

    alphas match {
      case Some(alphaValues) => // with alpha channel
        (reds, greens, blues).zipped.toIndexedSeq.zip(alphaValues).map { case ((r, g, b), a) => RGBA(r, g, b, a) }
      case None => // RGB only
        (reds, greens, blues).zipped.toIndexedSeq.map { case (r, g, b) => RGBA(r, g, b) }
    }
  }

  def getNormals(properties: List[(String, List[_])]): IndexedSeq[Vector[_3D]] = {
    val nx = properties.find(e => e._1 == PLY.nx).getOrElse(
      throw new IOException("Could not read normals x component.")
    )._2.map(e => any2Double(e))

    val ny = properties.find(e => e._1 == PLY.ny).getOrElse(
      throw new IOException("Could not read normals y component.")
    )._2.map(e => any2Double(e))

    val nz = properties.find(e => e._1 == PLY.nz).getOrElse(
      throw new IOException("Could not read normals z component.")
    )._2.map(e => any2Double(e))

    (nx, ny, nz).zipped.toIndexedSeq.map(t => Vector(t._1, t._2, t._3))
  }


  def getTextureCoordinates(properties: List[(String, List[_])]): IndexedSeq[Point[_2D]] = {
    val um = properties.find(e => e._1 == PLY.meshlabU).orNull
    val vm = properties.find(e => e._1 == PLY.meshlabV).orNull
    if ((um != null) && (vm != null)) {
      toTextureCoordinates(um, vm)
    } else {
      val ub = properties.find(e => e._1 == PLY.blenderU)
      val vb = properties.find(e => e._1 == PLY.blenderV)
      val texCoords = for (u <- ub; v <- vb) yield toTextureCoordinates(u, v)
      texCoords.getOrElse(throw new IOException("Could not read texture coordinates."))
    }
  }

  def toTextureCoordinates(um: (String, List[_]), vm: (String, List[_])): IndexedSeq[Point[_2D]] = {
    val u = um._2.map(e => any2Double(e))
    val v = vm._2.map(e => any2Double(e))
    u.zip(v).map(t => Point(t._1, t._2)).toIndexedSeq
  }

  def getTriTextureCoordinates(properties: List[(String, List[_])]): IndexedSeq[Seq[Point[_2D]]] = {
    val t = properties.find(e => e._1 == PLY.textureCoordinates).getOrElse(
      throw new IOException("Could not read per triangle vertex texture coordinates.")
    )._2.grouped(6).map(e => listOfAny2ListOfPoint2D(e)).toIndexedSeq
    t
  }

  def getTriangleVertexNormals(properties: List[(String, List[_])]): IndexedSeq[Seq[Vector[_3D]]] = {
    val t = properties.find(e => e._1 == PLY.normals).getOrElse(
      throw new IOException("Could not read per triangle vertex normals coordinates.")
    )._2.grouped(9).map(e => listOfAny2ListOfVector3D(e)).toIndexedSeq
    t
  }

  def getSurfaceColor(vertexProperties: List[(String, List[_])],
                      faceProperties: List[(String, List[_])],
                      texture: List[PixelImage[RGBA]],
                      triangleList: TriangleList
                     ): MeshSurfaceProperty[RGBA] = {


    val texCoordsTriangleVertex = try {
      Some(getTriTextureCoordinates(faceProperties))
    } catch {
      case _: Throwable => None
    }

    if (texCoordsTriangleVertex.isDefined && texture.nonEmpty) {
      val triangleIndex = (0 until triangleList.triangles.size * 3).grouped(3).map(g => IntVector3D(g(0), g(1), g(2))).toIndexedSeq
      val prop = VertexPropertyPerTriangle(triangleList, triangleIndex, texCoordsTriangleVertex.get.flatten)
      TextureMappedProperty(triangleList, prop, texture.head)
    } else {

      val texCoordsVertex = try {
        Some(getTextureCoordinates(vertexProperties))
      } catch {
        case NonFatal(e) => None
      }

      if (texCoordsVertex.isDefined && texture.nonEmpty) {
        val prop = SurfacePointProperty(triangleList, texCoordsVertex.get)
        TextureMappedProperty(triangleList, prop, texture.head)
      } else {
        val vertexColors = try {
          Some(getColors(vertexProperties))
        } catch {
          case NonFatal(e) => None
        }

        if (vertexColors.isDefined) {
          SurfacePointProperty(triangleList, vertexColors.get)
        } else {
          val triangleColors = try {
            Some(getColors(faceProperties))
          } catch {
            case NonFatal(e) => None
          }
          if (triangleColors.isDefined) {
            TriangleProperty(triangleList, triangleColors.get)
          } else {
            throw new IOException("Could not read any surface color.")
          }
        }
      }
    }
  }


  def getSurfaceNormal(vertexProperties: List[(String, List[_])],
                       faceProperties: List[(String, List[_])],
                       texture: List[PixelImage[RGBA]],
                       triangleList: TriangleList
                      ): MeshSurfaceProperty[Vector[_3D]] = {

    val normalPerTriangleVertex = try {
      Some(getTriangleVertexNormals(faceProperties))
    } catch {
      case _: Throwable => None
    }

    if (normalPerTriangleVertex.isDefined) {
      val triangleIndex = (0 until triangleList.triangles.size * 3).grouped(3).map(g => IntVector3D(g(0), g(1), g(2))).toIndexedSeq
      val prop = VertexPropertyPerTriangle(triangleList, triangleIndex, normalPerTriangleVertex.get.flatten)
      prop
    } else {
      val vertexNormals = try {
        Some(getNormals(vertexProperties))
      } catch {
        case _: Throwable => None
      }

      if (vertexNormals.isDefined) {
        SurfacePointProperty(triangleList, vertexNormals.get)
      } else {
        val triangleNormals = try {
          Some(getNormals(faceProperties))
        } catch {
          case _: Throwable => None
        }
        if (triangleNormals.isDefined) {
          TriangleProperty(triangleList, triangleNormals.get)
        } else {
          throw new IOException("Could not read any surface color.")
        }
      }
    }
  }

}


case class PlyElementReader(N: Int, readers: List[(String, PlyPropertyReader[_])]) {

  def read(is: InputStream, bo: ByteOrder): List[(String, List[_])] = {
    val readerList = readers.map(_._2)
    for (i <- 0 until N) {
      readerList.foreach(r => r.read(is, bo))
    }
    readers.map(e => (e._1, e._2.getList))
  }

  def read(scanner: Scanner): List[(String, List[_])] = {
    for (i <- 0 until N) {
      readers.foreach(r => r._2.read(scanner))
    }
    readers.map(e => (e._1, e._2.getList))
  }

}

class PlyPropertyReader[A](private val reader: SequenceReader[A]) {

  private val _buffer: ListBuffer[A] = new ListBuffer[A]

  def getList: List[A] = _buffer.toList

  def read(scanner: Scanner): Seq[A] = {
    _buffer ++= reader.read(scanner)
  }

  def read(is: InputStream, bo: ByteOrder): Seq[A] = {
    _buffer ++= reader.read(is, bo)
  }

}


