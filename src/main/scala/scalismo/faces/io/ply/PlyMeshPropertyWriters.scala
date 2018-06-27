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

import java.io.{OutputStream, OutputStreamWriter}
import java.nio.ByteOrder

import scalismo.common.PointId
import scalismo.faces.color.RGBA
import scalismo.faces.mesh.VertexPropertyPerTriangle
import scalismo.geometry.{_2D, _}
import scalismo.mesh.{BarycentricCoordinates, SurfacePointProperty, TriangleId, TriangleProperty}

/**
  * The object PLYPropertyWriters is a collection of helpers to write a property to a file.
  *
  * The base class is IndexedProperty.
  * On next more concrete level we have the SurfacePointPropertyWriter, TrianglePropertyWriter and VertexPerTrianglePropertyWriter.
  * Finally the different properties occurring in a mesh are written using the remaining classes in this file.
  */
object PlyMeshPropertyWriters {

  import PlyHelpers.PlyHeader._
  import PlyHelpers._
  import PlyMeshHelper._

  trait IndexedProperty {
    def writeHeader(osw:OutputStreamWriter)
    def write(fIdx: Int, osw: OutputStreamWriter)
    def write(fIdx: Int, os: OutputStream, bo: ByteOrder)
  }

  abstract class SurfacePointPropertyWriter[A] extends IndexedProperty{
    def property: SurfacePointProperty[A]

    def write(a: A, osw: OutputStreamWriter)
    def write(a: A, os: OutputStream, bo: ByteOrder)

    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      write(property.atPoint(PointId(idx)),osw)
    }
    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      write(property.atPoint(PointId(idx)),os,bo)
    }
  }

  abstract class VertexPoint[D <: Dim](override val property: SurfacePointProperty[Point[D]] )
    extends SurfacePointPropertyWriter[Point[D]]
  {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]()

    override def write(vec: Point[D], osw: OutputStreamWriter): Unit = {
      writer.write(vec.toArray.map(_.toFloat), osw)
    }

    override def write(vec: Point[D], os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vec.toArray.map(_.toFloat), os, bo)
    }
  }

  abstract class VertexVector[D <: Dim](override val property: SurfacePointProperty[Vector[D]] ) extends SurfacePointPropertyWriter[Vector[D]] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]()

    override def write(vec: Vector[D], osw: OutputStreamWriter): Unit = {
      writer.write(vec.toArray.map(_.toFloat), osw)
    }

    override def write(vec: Vector[D], os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vec.toArray.map(_.toFloat), os, bo)
    }
  }

  abstract class TrianglePropertyWriter[A] extends IndexedProperty{
    def property: TriangleProperty[A]
    def write(a: A, osw: OutputStreamWriter)
    def write(a: A, os: OutputStream, bo: ByteOrder)
    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      write(property(TriangleId(idx)),osw)
    }
    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      write(property(TriangleId(idx)),os,bo)
    }
  }

  abstract class VertexPerTrianglePropertyWriter[A] extends IndexedProperty {
    def property: VertexPropertyPerTriangle[A]
    def write(a: Seq[A], osw: OutputStreamWriter)
    def write(a: Seq[A], os: OutputStream, bo: ByteOrder)
    override def write( idx: Int, osw: OutputStreamWriter): Unit = {
      val a = property(TriangleId(idx),BarycentricCoordinates.v0)
      val b = property(TriangleId(idx),BarycentricCoordinates.v1)
      val c = property(TriangleId(idx),BarycentricCoordinates.v2)
      write(IndexedSeq(a,b,c),osw)
    }
    override def write( idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      val a = property(TriangleId(idx),BarycentricCoordinates.v0)
      val b = property(TriangleId(idx),BarycentricCoordinates.v1)
      val c = property(TriangleId(idx),BarycentricCoordinates.v2)
      write(IndexedSeq(a,b,c),os,bo)
    }
  }

  abstract class FaceVertexVectors[D<:Dim]( override val property: VertexPropertyPerTriangle[Vector[D]]) extends VertexPerTrianglePropertyWriter[Vector[D]] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new ListWriter[Float]

    override def write(seq: Seq[Vector[D]], osw: OutputStreamWriter): Unit = {
      val s = seq.flatMap(_.toArray).map(_.toFloat)
      writer.write(s,osw)
    }

    override def write(seq: Seq[Vector[D]], os: OutputStream, bo: ByteOrder): Unit = {
      val s = seq.flatMap(_.toArray).map(_.toFloat)
      writer.write(s,os,bo)
    }

  }

  class Vertex(val vertices: IndexedSeq[Point[_3D]]) extends IndexedProperty {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]

    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      writer.write(vertices(idx).toArray.map(_.toFloat),osw)
    }

    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vertices(idx).toArray.map(_.toFloat),os,bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.xCoordinate))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.yCoordinate))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.zCoordinate))
    }
  }

  class VertexColor(override val property: SurfacePointProperty[RGBA] ) extends SurfacePointPropertyWriter[RGBA] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.uchar
    private val writer = new SequenceWriter[Byte]

    override def write(color: RGBA, osw: OutputStreamWriter): Unit = {
      val clamped = color.clamped
      val seq = IndexedSeq(clamped.r,clamped.g,clamped.b,clamped.a).map(zeroOne2Byte)
      writer.write(seq,osw)
    }

    override def write(color: RGBA, os: OutputStream, bo: ByteOrder): Unit = {
      val clamped = color.clamped
      val seq = IndexedSeq(clamped.r,clamped.g,clamped.b,clamped.a).map(zeroOne2Byte)
      writer.write(seq,os,bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.red))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.green))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.blue))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.alpha))
    }
  }

  class VertexNormal(property: SurfacePointProperty[Vector[_3D]])
    extends VertexVector[_3D](property)
  {
    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.nx))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.ny))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.nz))
    }
  }

  class VertexTextureCoordinates(property: SurfacePointProperty[Point[_2D]], format : PlyHeader = PlyHeader.meshlab)
    extends VertexPoint[_2D](property)
  {
    val headerFormat: PlyHeader = format
    override def writeHeader(osw: OutputStreamWriter): Unit = {
      headerFormat match {
        case PlyHeader.meshlab =>
          osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.meshlabU))
          osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.meshlabV))
        case PlyHeader.blender =>
          osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.blenderU))
          osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.blenderV))
      }
    }
  }

  class Faces(val faces: IndexedSeq[IntVector[_3D]])
    extends IndexedProperty
  {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.int
    private val writer = new ListWriter[Int]

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s %s %s\n".format(PLY.property,PLY.list,PlyTypes.uchar,numberFormat,PLY.vertexIndices))
    }

    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      writer.write(faces(idx).toArray,osw)
    }

    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(faces(idx).toArray,os,bo)
    }
  }

  class FaceColor( override val property: TriangleProperty[RGBA]) extends TrianglePropertyWriter[RGBA] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.uchar
    private val writer = new SequenceWriter[Byte]()

    override def write(color: RGBA, osw: OutputStreamWriter): Unit = {
      val seq = IndexedSeq(color.r,color.g,color.b,color.a).map(zeroOne2Byte)
      writer.write(seq,osw)
    }

    override def write(color: RGBA, os: OutputStream, bo: ByteOrder): Unit = {
      val seq = IndexedSeq(color.r,color.g,color.b,color.a).map(zeroOne2Byte)
      writer.write(seq,os,bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.red))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.green))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.blue))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.alpha))
    }
  }

  class FaceNormal( override val property: TriangleProperty[Vector[_3D]]) extends TrianglePropertyWriter[Vector[_3D]] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]()

    override def write(vec: Vector[_3D], osw: OutputStreamWriter): Unit = {
      writer.write(vec.toArray.map(_.toFloat), osw)
    }

    override def write(vec: Vector[_3D], os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vec.toArray.map(_.toFloat), os, bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.nx))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.ny))
      osw.write("%s %s %s\n".format(PLY.property,numberFormat,PLY.nz))
    }
  }

  class FaceVertexTextureCoordinates( override val property: VertexPropertyPerTriangle[Point[_2D]]) extends VertexPerTrianglePropertyWriter[Point[_2D]] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new ListWriter[Float]

    override def write(seq: Seq[Point[_2D]], osw: OutputStreamWriter): Unit = {
      val s = seq.flatMap(_.toArray).map(_.toFloat)
      writer.write(s,osw)
    }

    override def write(seq: Seq[Point[_2D]], os: OutputStream, bo: ByteOrder): Unit = {
      val s = seq.flatMap(_.toArray).map(_.toFloat)
      writer.write(s,os,bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s %s %s\n".format(PLY.property,PLY.list,PlyTypes.uchar,numberFormat,PLY.textureCoordinates))
    }

  }

  class FaceVertexColors( override val property: VertexPropertyPerTriangle[RGBA]) extends VertexPerTrianglePropertyWriter[RGBA] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.uchar
    private val writer = new ListWriter[Byte]

    override def write(seq: Seq[RGBA], osw: OutputStreamWriter): Unit = {
      val s = seq.flatMap(c => Seq(c.r,c.g,c.b,c.a)).map(zeroOne2Byte)
      writer.write(s,osw)
    }

    override def write(seq: Seq[RGBA], os: OutputStream, bo: ByteOrder): Unit = {
      val s = seq.flatMap(c => Seq(c.r,c.g,c.b,c.a)).map(zeroOne2Byte)
      writer.write(s,os,bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s %s %s\n".format(PLY.property,PLY.list,PlyTypes.uchar,numberFormat,PLY.vertexColor))
    }
  }

  class FaceVertexNormals(override val property: VertexPropertyPerTriangle[Vector[_3D]]) extends FaceVertexVectors[_3D](property) {
    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s %s %s\n".format(PLY.property,PLY.list,PlyTypes.uchar,numberFormat,PLY.normals))
    }
  }







  def groupByKey[K: scala.reflect.ClassTag, V: scala.reflect.ClassTag](arr: Array[(K, V)]): Array[(K, Array[V])] = {
    val list = new scala.collection.mutable.ArrayBuffer[(K, Array[V])]()
    val innerList = new scala.collection.mutable.ArrayBuffer[V]()
    var key = arr(0)._1
    arr.foreach { p =>
      val k = p._1
      val v = p._2
      if (k != key) {
        list.append((key, innerList.toArray.clone()))
        key = k
        innerList.clear()
        innerList += v
      } else {
        innerList += v
      }
    }
    if (arr.length > 0) list.append((key, innerList.toArray.clone()))
    list.toArray
  }

  def sortedInverseIndex(arr: Array[IntVector[_3D]]): Array[(Int, Array[Int])] = {
    val flatIdx = arr.flatMap(_.toArray).zipWithIndex.sortBy(a => a._1)
    groupByKey[Int, Int](flatIdx)
  }

  def checkPerVertexProperty(property: Array[IntVector[_3D]], referenceIndex: Array[IntVector[_3D]]): Boolean = {
    val inverseIndex = sortedInverseIndex(referenceIndex)
    val testIndex = property.flatMap(_.toArray)
    inverseIndex.forall { ii =>
      //      val vertexIndex = ii._1
      val flatIndices = ii._2
      flatIndices.map(testIndex).toSet.size == 1
    }
  }


}
