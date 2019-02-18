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
import java.nio.ByteBuffer
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scalismo.color.RGBA
import scalismo.faces.io.PixelImageIO
import scalismo.faces.utils.ResourceManagement
import scalismo.geometry.{IntVector, Point, EuclideanVector, _2D, _3D}

import scala.reflect.ClassTag
import scala.util.control.NonFatal

/** read and write Gravis binary mesh format MSH - use PLY for general use */
object GravisMSHFormat {
  // magic values
  val magicStart = "GRAVIS::MSH2::BINARY::START\n"
  val magicEnd = "\nGRAVIS::MSH2::BINARY::END\n"


  /** encapsulate all MSH binary reading methods */
  object Reader {
    // MSH is in little endian format while Java uses big endian ...
    private def readBytesAsLE(ds: DataInputStream, n: Int): Array[Byte] = {
      readBytesAsBE(ds, n).reverse
    }

    private def readBytesAsBE(ds: DataInputStream, n: Int): Array[Byte] = {
      val buffer = new Array[Byte](n)
      readExactlyNBytes(ds, buffer)
      buffer
    }

    /** helper method to ensure reading of the full buffer, InputStream.read does not ensure this */
    private def readExactlyNBytes(ds: DataInputStream, buffer: Array[Byte]): Unit = {
      val n = buffer.length
      var read = 0
      while (read < n) {
        read += ds.read(buffer, read, n - read)
      }
    }

    private def readBoolean(ds: DataInputStream): Boolean = {
      ds.readBoolean()
    }

    private def readFloat(ds: DataInputStream): Float = {
      val b = readBytesAsLE(ds, 4)
      ByteBuffer.wrap(b).getFloat
    }

    private def readInt(ds: DataInputStream): Int = {
      val b = readBytesAsLE(ds, 4)
      ByteBuffer.wrap(b).getInt
    }

    private def readSize(ds: DataInputStream): Int = {
      val size = readInt(ds) // TODO read uint32 rather than int32
      require(size >= 0, "binary MSH reader: size is invalid (probably too large, overflow)")
      size
    }

    private def readIndexedSeq[T: ClassTag](elementReader: DataInputStream => T)(ds: DataInputStream): Array[T] = {
      val size = readSize(ds)
      Array.fill(size)(elementReader(ds))
    }

    private def readString(ds: DataInputStream): String = {
      val len = readSize(ds)
      val buffer = readBytesAsBE(ds, len)
      new String(buffer.map(_.toChar))
    }

    private def readMSHMaterial(path: File)(ds: DataInputStream): MSHMaterial = {
      val name = readString(ds)
      val ambient = readRGBA(ds) // ambient
      val diffuse = readRGBA(ds) // diffuse
      val specular = readRGBA(ds) // specular
      val shininess = readFloat(ds) // shininess
      val hasTexture = readBoolean(ds) // hasTexture
      val textureName = readString(ds) // textureName
      val texture = if (hasTexture) Some(MSHTexture.fromFile(new File(path, textureName))) else None

      MSHMaterial(name,
        ambient,
        diffuse,
        specular,
        shininess,
        texture)
    }

    private def readRGBA(ds: DataInputStream): RGBA = {
      val r = readFloat(ds)
      val g = readFloat(ds)
      val b = readFloat(ds)
      val a = readFloat(ds)
      RGBA(r, g, b, a)
    }

    private def readVector(ds: DataInputStream): EuclideanVector[_3D] = {
      val x = readFloat(ds)
      val y = readFloat(ds)
      val z = readFloat(ds)
      EuclideanVector(x, y, z)
    }

    private def readIntVector(ds: DataInputStream): IntVector[_3D] = {
      val i = readInt(ds)
      val j = readInt(ds)
      val k = readInt(ds)
      IntVector(i, j, k)
    }

    private def readIntVector2D(ds: DataInputStream): IntVector[_2D] = {
      val i = readInt(ds)
      val j = readInt(ds)
      IntVector(i, j)
    }

    private def checkMagicString(ds: DataInputStream, magic: String): Boolean = {
      val buffer = readBytesAsBE(ds, magic.length)
      val magicRead = new String(buffer.map(_.toChar))
      if (magic == magicRead)
        true
      else
        throw new IOException("MSH: magic string not found, format corrupt")
    }

    def readMSHMesh(file: File): MSHMesh = {
      ResourceManagement.using(openInflated(file)) { stream =>
        checkMagicString(stream, magicStart)
        // =mesh=
        val materials = readIndexedSeq(readMSHMaterial(file.getParentFile.getAbsoluteFile))(stream)
        val vertices = readIndexedSeq(readVector)(stream)
        val normals = readIndexedSeq(readVector)(stream)
        val textureCoordinates = readIndexedSeq(readVector)(stream)
        val colors = readIndexedSeq(readRGBA)(stream)
        // =triangles=
        val triangleList = readIndexedSeq(readIntVector)(stream)
        val tni = readIndexedSeq(readIntVector)(stream)
        val tti = readIndexedSeq(readIntVector)(stream)
        val tci = readIndexedSeq(readIntVector)(stream)
        val tmi = readIndexedSeq(readInt)(stream)
        // =lines=
        val lvi = readIndexedSeq(readIntVector2D)(stream)
        val lti = readIndexedSeq(readIntVector2D)(stream)
        val lci = readIndexedSeq(readIntVector2D)(stream)
        // =points=
        val pvi = readIndexedSeq(readInt)(stream)
        val pci = readIndexedSeq(readInt)(stream)
        // end magic
        checkMagicString(stream, magicEnd)

        val points = vertices.map(_.toPoint)
        val texCoords = textureCoordinates.map(_.toPoint: Point[_3D])
        MSHMesh(materials,
          points,
          normals,
          texCoords,
          colors,
          triangleList,
          tni,
          tti,
          tci,
          tmi,
          lvi,
          lti,
          lci,
          pvi,
          pci,
          file.getParentFile.getAbsoluteFile) // absolute path of containing directory
      }
    }

    private def openInflated(file: File): DataInputStream = {
      try {
        new DataInputStream(new GZIPInputStream(new FileInputStream(file)))
      } catch {
        case e: java.util.zip.ZipException => new DataInputStream(new BufferedInputStream(new FileInputStream(file))) // not zipped
        case NonFatal(e) => throw e
      }
    }

  }

  object Writer {
    /** open a file with transparent GZip compression */
    private def openDeflated(file: File): DataOutputStream = file.getPath.toLowerCase match {
      case fileName if fileName.endsWith(".gz") => new DataOutputStream(new GZIPOutputStream(new FileOutputStream(file)))
      case _ => new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    }

    // MSH is in little endian format while Java uses big endian ...
    private def writeBytesAsLE(ds: DataOutputStream, data: Array[Byte]): Unit = writeBytesAsBE(ds, data.reverse)

    private def writeBytesAsBE(ds: DataOutputStream, data: Array[Byte]): Unit = ds.write(data)

    private def writeFloat(ds: DataOutputStream, data: Float) = {
      val buffer = ByteBuffer.allocate(4).putFloat(data).array()
      writeBytesAsLE(ds, buffer)
    }

    private def writeInt(ds: DataOutputStream, data: Int) = {
      val buffer = ByteBuffer.allocate(4).putInt(data).array()
      writeBytesAsLE(ds, buffer)
    }

    private def writeBoolean(ds: DataOutputStream, data: Boolean) = {
      ds.writeBoolean(data)
    }

    private def writeIndexedSeq[T](elementWriter: (DataOutputStream, T) => Unit)(ds: DataOutputStream, data: IndexedSeq[T]) = {
      val size: Int = data.length
      writeInt(ds, size)
      data.foreach(elementWriter(ds, _))
    }

    private def writeString(ds: DataOutputStream, data: String) = {
      writeInt(ds, data.length)
      writeBytesAsBE(ds, data.toCharArray.map(_.toByte))
    }

    private def writeMagicString(ds: DataOutputStream, data: String) = {
      writeBytesAsBE(ds, data.toCharArray.map(_.toByte))
    }

    private def writePoint(ds: DataOutputStream, data: Point[_3D]) = {
      writeFloat(ds, data.x.toFloat)
      writeFloat(ds, data.y.toFloat)
      writeFloat(ds, data.z.toFloat)
    }

    private def writeVector(ds: DataOutputStream, data:EuclideanVector[_3D]) = {
      writeFloat(ds, data.x.toFloat)
      writeFloat(ds, data.y.toFloat)
      writeFloat(ds, data.z.toFloat)
    }

    private def writeRGBA(ds: DataOutputStream, data: RGBA) = {
      writeFloat(ds, data.r.toFloat)
      writeFloat(ds, data.g.toFloat)
      writeFloat(ds, data.b.toFloat)
      writeFloat(ds, data.a.toFloat)
    }

    private def writeIntVector(ds: DataOutputStream, data: IntVector[_3D]): Unit = {
      writeInt(ds, data.i)
      writeInt(ds, data.j)
      writeInt(ds, data.k)
    }

    private def writeIntVector2D(ds: DataOutputStream, data: IntVector[_2D]): Unit = {
      writeInt(ds, data.i)
      writeInt(ds, data.j)
    }

    private def writeMSHMaterial(meshFile: File)(ds: DataOutputStream, data: MSHMaterial) = {
      writeString(ds, data.name)
      writeRGBA(ds, data.ambient)
      writeRGBA(ds, data.diffuse)
      writeRGBA(ds, data.specular)
      writeFloat(ds, data.shininess.toFloat)

      data.texture match {
        case Some(tex: MSHTexture) =>
          writeBoolean(ds, data = true)
          val meshName = meshFile.getName.replaceFirst("[.]gz$", "").replaceFirst("[.][^.]+$", "")
          val meshPath = meshFile.getAbsoluteFile.getParent
          val textureName = if (tex.file.getName.nonEmpty) meshName + "_" + tex.file.getName else meshName + "_" + data.name.replaceAll("[^a-zA-Z0-9.-]", "_") + ".png"
          val textureFile = new File(meshPath, textureName)
          writeString(ds, textureFile.getName)
          PixelImageIO.write(tex.image, textureFile).get
        case None =>
          writeBoolean(ds, data = false)
          writeString(ds, "")
      }
    }

    def writeMSHMesh(mesh: MSHMesh, file: File): Unit = {
      ResourceManagement.using(openDeflated(file)) { stream =>
        writeMagicString(stream, magicStart)
        // =mesh=
        writeIndexedSeq(writeMSHMaterial(file))(stream, mesh.materials)
        writeIndexedSeq(writePoint)(stream, mesh.vertex)
        writeIndexedSeq(writeVector)(stream, mesh.normal)
        writeIndexedSeq(writePoint)(stream, mesh.textureCoordinates)
        writeIndexedSeq(writeRGBA)(stream, mesh.color)
        // =triangles=
        writeIndexedSeq(writeIntVector)(stream, mesh.tvi)
        writeIndexedSeq(writeIntVector)(stream, mesh.tni)
        writeIndexedSeq(writeIntVector)(stream, mesh.tti)
        writeIndexedSeq(writeIntVector)(stream, mesh.tci)
        writeIndexedSeq(writeInt)(stream, mesh.tmi)
        // =lines=
        writeIndexedSeq(writeIntVector2D)(stream, mesh.lvi)
        writeIndexedSeq(writeIntVector2D)(stream, mesh.lti)
        writeIndexedSeq(writeIntVector2D)(stream, mesh.lci)
        // =points=
        writeIndexedSeq(writeInt)(stream, mesh.pvi)
        writeIndexedSeq(writeInt)(stream, mesh.pci)
        // end magic
        writeMagicString(stream, magicEnd)
      }
    }
  }

}
