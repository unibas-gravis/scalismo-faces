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

import java.io.{File, FileOutputStream, OutputStream, OutputStreamWriter}
import java.nio.ByteOrder

import scalismo.color.RGBA
import scalismo.faces.io.PixelImageIO
import scalismo.faces.io.ply.PlyHelpers.PlyFormat._
import scalismo.faces.io.ply.PlyHelpers.PlyHeader._
import scalismo.faces.io.ply.PlyHelpers._
import scalismo.faces.io.ply.PlyMeshPropertyWriters._
import scalismo.faces.mesh.{IndirectProperty, TextureMappedProperty, VertexPropertyPerTriangle}
import scalismo.geometry._
import scalismo.mesh._

import scala.reflect.io.Path


/**
  * Writes a ply file based on the passed arguments. The arguments are first analyzed and the
  * nescessary writer-chains are built for the vertex and face properties. The writer are in
  * PlyMeshPropertyWriters. The writers make use of the mesh-independant List- and SequenceWriters.
  *
  * @param url
  * @param vertices
  * @param faces
  * @param color
  * @param normals
  * @param plyFormat
  * @param headerFormat
  */
private[io] case class PlyMeshWriter(url: String,
                                     vertices: Option[IndexedSeq[Point[_3D]]] = None,
                                     faces: Option[IndexedSeq[IntVector[_3D]]] = None,
                                     color: Option[MeshSurfaceProperty[RGBA]] = None,
                                     normals: Option[MeshSurfaceProperty[EuclideanVector[_3D]]] = None,
                                     plyFormat: PlyFormat = PlyFormat.ASCII,
                                     headerFormat: PlyHeader = PlyHeader.meshlab) {

  val nVertices: Int = vertices.map(_.length).getOrElse(-1)
  val nFaces: Int = faces.map(_.length).getOrElse(-1)
  val (hasTextures, hasMultipleTextures) = checkTextures

  val vertexProperties: IndexedSeq[IndexedProperty] = getVertexProperties
  val faceProperties: IndexedSeq[IndexedProperty] = getFaceProperties


  def writeToStream(os: OutputStream): Unit = {
    val osw = new OutputStreamWriter(os)
    writeHeader(osw)
    osw.flush()
    writeData(os)
    osw.flush()
  }

  def writeToFile(file: File): Unit = {
    writeToStream(new FileOutputStream(file))
  }

  def write(filename: String): Unit = {
    val plyFile = new File(filename)
    writeToFile(plyFile)
  }

  private def getVertexProperties: IndexedSeq[IndexedProperty] = {

    val _vertices = vertices.map(new Vertex(_))

    val _vColors = color.flatMap {
      case c: SurfacePointProperty[RGBA] => Some(new VertexColor(c))
      case _ => None
    }

    val _vNormals = normals.flatMap {
      case n: SurfacePointProperty[EuclideanVector[_3D]] => Some(new VertexNormal(n))
      case n: MappedSurfaceProperty[EuclideanVector[_3D],_] => {
        // fully evaluate the lazy mapped surface property
        val surfaceProp =  SurfacePointProperty.averagedPointProperty(n)
        Some(new VertexNormal(surfaceProp))
      }
      case _ => None
    }

    val (_vTextureCoordinates, _vTextureIndex) = getTextureCoordinates

    IndexedSeq(
      _vertices,
      _vNormals,
      _vColors,
      _vTextureIndex,
      _vTextureCoordinates
    ).flatten
  }

  private def checkTextures: (Boolean, Boolean) = {
    color match {
      case Some(_) => color.get match {
        case _: TextureMappedProperty[RGBA] => (true, false)
        case ip: IndirectProperty[RGBA] =>
          val hmt = ip.properties.forall {
            case _: TextureMappedProperty[RGBA] => true
            case _ => false
          }
          (hmt, hmt)
        case _ => (false, false)
      }
      case None => (false, false)
    }
  }

  private def getTextureCoordinates: (Option[VertexTextureCoordinates], None.type) = {
    color match {
      case Some(_) => color.get match {
        case tmp: TextureMappedProperty[RGBA] => tmp.textureMapping match {
          case tc: SurfacePointProperty[Point[_2D]] => (Some(new VertexTextureCoordinates(tc, headerFormat)), None)
          case _ => (None, None)
        }
        //        // TODO: FIX THIS CASE
        //        case ip: IndirectProperty[RGBA] => {
        //          val test = ip.properties.forall {
        //            case tmp: TextureMappedProperty[RGBA] => tmp.textureMapping match {
        //              case _: SurfacePointProperty[Point[_2D]] => true
        //              case _ => false
        //              case _ => false
        //            }
        //          }
        //        }
        case _ => (None, None)
      }
      case None => (None, None)
    }
  }


  private def getFaceProperties: IndexedSeq[IndexedProperty] = {
    val _faces = faces.map(new Faces(_))

    val _faceColor = color.flatMap {
      case color: TriangleProperty[RGBA] => Some(new FaceColor(color))
      case _ => None
    }

    val _vertexColorPerFace = color.flatMap {
      case color: VertexPropertyPerTriangle[RGBA] => Some(new FaceVertexColors(color))
      case _ => None
    }

    val _faceNormals = normals.flatMap {
      case tn: TriangleProperty[EuclideanVector[_3D]] => Some(new FaceNormal(tn))
      case _ => None
    }

    val _vertexNormalsPerFace = normals.flatMap {
      case vn: VertexPropertyPerTriangle[EuclideanVector[_3D]] => Some(new FaceVertexNormals(vn))
      case _ => None
    }

    val _vertexTextureCoordinatesPerFace = color.flatMap {
      case tmp: TextureMappedProperty[RGBA] => tmp.textureMapping match {
        case vppt: VertexPropertyPerTriangle[Point[_2D]] => Some(new FaceVertexTextureCoordinates(vppt))
        case _ => None
      }
      //        // TODO: FIX THIS CASE
      //        case ip: IndirectProperty[RGBA] => ip.properties.forall {
      //          case tmp: TextureMappedProperty[RGBA] => tmp.textureMapping match {
      //            case _: VertexPropertyPerTriangle[Point[_2D]] => true
      //            case _ => false
      //          }
      //          case _ => false
      //        }
      case _ => None
    }

    IndexedSeq(
      _faces,
      _faceColor,
      _faceNormals,
      _vertexTextureCoordinatesPerFace,
      _vertexColorPerFace,
      _vertexNormalsPerFace
    ).flatten
  }

  private def writeHeader(osw: OutputStreamWriter) {
    osw.write("ply\n")
    osw.write("format %s 1.0\n".format(plyFormat))
    osw.write("comment Created by GraVis-Faces\n")

    if (hasTextures) {
      writeTextures(osw)
      osw.flush()
    }

    if (nVertices > 0) {
      writeVertexHeader(osw)
      osw.flush()
    }

    if (nFaces > 0) {
      writeFacesHeader(osw)
      osw.flush()
    }

    osw.write("end_header\n")
    osw.flush()
  }

  private def writeTextures(osw: OutputStreamWriter): Unit = {
    val textureDir = Path(url).parent
    val textureBaseName = Path(url).name.toString
    val textureNames = color match {
      case Some(c: TextureMappedProperty[RGBA]) =>
        val filename = "%s.png".format(textureBaseName)
        PixelImageIO.write(c.texture, new File((textureDir / filename).toString))
        IndexedSeq(filename)
      case Some(ip: IndirectProperty[RGBA]) =>
        val textures = ip.properties.map {
          case t: TextureMappedProperty[RGBA] => t.texture
          case _ => throw new RuntimeException("Writing multiple SurfacePointProperties is only supported for TextureMappedProperties!")
        }
        textures.zipWithIndex.map { ti =>
          val t = ti._1
          val i = ti._2
          val filename = "%s_%d.png".format(textureBaseName, i)
          PixelImageIO.write(t, new File((textureDir / filename).toString))
          filename
        }
      case _ => IndexedSeq[String]()
    }
    textureNames.foreach { name =>
      osw.write(PLY.comment + " " + PLY.textureFile + " %s\n".format(name))
    }
    osw.flush()
  }

  private def writeVertexHeader(osw: OutputStreamWriter): Unit = {
    osw.write(PLY.element + " " + PLY.vertex + " %d\n".format(nVertices))
    vertexProperties.foreach { vp =>
      vp.writeHeader(osw)
    }
    osw.flush()
  }

  private def writeFacesHeader(osw: OutputStreamWriter): Unit = {
    osw.write(PLY.element + " " + PLY.face + " %d\n".format(nFaces))
    faceProperties.foreach { fp =>
      fp.writeHeader(osw)
    }
    osw.flush()
  }


  private def writeData(os: OutputStream): Unit = {
    if (nVertices > 0) {
      writeVertexData(os)
      os.flush()
    }
    if (nFaces > 0) {
      writeFaceData(os)
      os.flush()
    }
  }

  private def writeVertexData(os: OutputStream): Unit = {
    plyFormat match {
      case PlyFormat.ASCII =>
        val osw = new OutputStreamWriter(os)
        (0 until nVertices).foreach { vIdx =>
          vertexProperties.head.write(vIdx, osw)
          vertexProperties.tail.foreach { vp =>
            osw.write(" ")
            vp.write(vIdx, osw)
          }
          osw.write("\n")
        }
        osw.flush()
      case PlyFormat.BinaryLittleEndian =>
        (0 until nVertices).foreach { vIdx =>
          vertexProperties.foreach(_.write(vIdx, os, ByteOrder.LITTLE_ENDIAN))
          os.flush()
        }
      case PlyFormat.BinaryBigEndian =>
        (0 until nVertices).foreach { vIdx =>
          vertexProperties.foreach(_.write(vIdx, os, ByteOrder.BIG_ENDIAN))
          os.flush()
        }
    }
  }

  private def writeFaceData(os: OutputStream): Unit = {
    plyFormat match {
      case PlyFormat.ASCII =>
        val osw = new OutputStreamWriter(os)
        (0 until nFaces).foreach { fIdx =>
          faceProperties.head.write(fIdx, osw)
          faceProperties.tail.foreach { fp =>
            osw.write(" ")
            fp.write(fIdx, osw)
          }
          osw.write("\n")
        }
        osw.flush()
      case PlyFormat.BinaryLittleEndian =>
        (0 until nFaces).foreach { vIdx =>
          faceProperties.foreach(_.write(vIdx, os, ByteOrder.LITTLE_ENDIAN))
          os.flush()
        }
      case PlyFormat.BinaryBigEndian =>
        (0 until nFaces).foreach { vIdx =>
          faceProperties.foreach(_.write(vIdx, os, ByteOrder.BIG_ENDIAN))
          os.flush()
        }
    }
  }

}