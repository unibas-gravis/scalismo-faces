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

import java.io.File

import scalismo.faces.FacesTestSuite
import scalismo.faces.color.RGBA
import scalismo.faces.io.msh.{MSHMaterial, MSHMesh, MSHMeshIO, MSHTexture}
import scalismo.geometry._

class MSHMeshIOTests extends FacesTestSuite {
  // create a random mesh, write and read it, compare
  val gridMesh = randomGridMesh(3,3)

  val mshMesh = MSHMesh.fromTriangleMesh3D(gridMesh.shape, Some(gridMesh.color), Some(gridMesh.shape.vertexNormals))

  describe("A random triangle mesh converted to an MSH mesh with colors and normals") {
    it("should be equal to itself") {
      mshMesh shouldBe mshMesh
    }

    it("should be equal to a clone of itself") {
      mshMesh.clone() shouldBe mshMesh
    }

    it("should convert back to a VertexColorMesh") {
      val tMesh = mshMesh.vertexColorMesh.get
      tMesh.shape.triangulation shouldBe gridMesh.shape.triangulation
      tMesh.shape shouldBe gridMesh.shape
      tMesh.color.pointData.length shouldBe gridMesh.color.pointData.length
      val diff: Double = tMesh.color.pointData.zip(gridMesh.color.pointData).map { case (a, b) => {
        val c = a - b; c.dot(c)
      }
      }.sum
      diff should be < 1e-4
    }

    // temp file
    val f = File.createTempFile("mshtmp-gravismeshio-test", ".msh.gz")
    f.deleteOnExit()

    it("can be written to a file") {
      MSHMeshIO.write(mshMesh, f).get
    }

    it("can be read from a file unaltered") {
      val mshRead = MSHMeshIO.read(f).get


      // NOTE: simulate the loss of precision when writing using floats and reading back in doubles
      def rgbaToFloatToDouble = (c: RGBA) => {
        RGBA(c.r.toFloat.toDouble, c.g.toFloat.toDouble, c.b.toFloat.toDouble, c.a.toFloat.toDouble)
      }
      def pointToFloatToDouble = (p: Point[_3D]) => {
        Point3D(p(0).toFloat.toDouble, p(1).toFloat.toDouble, p(2).toFloat.toDouble)
      }
      def vectorToFloatToDouble = (v: Vector[_3D]) => {
        Vector3D(v(0).toFloat.toDouble, v(1).toFloat.toDouble, v(2).toFloat.toDouble)
      }
      def textureToFloatToDouble = (t: Option[MSHTexture]) => {
        t match {
          case Some(tex) => Option(MSHTexture(tex.image.map(c => rgbaToFloatToDouble(c)), tex.file))
          case None => None
        }
      }
      def materialToFloatToDouble = (m: MSHMaterial) => {
        MSHMaterial(
          name = m.name,
          ambient = rgbaToFloatToDouble(m.ambient),
          diffuse = rgbaToFloatToDouble(m.diffuse),
          specular = rgbaToFloatToDouble(m.specular),
          shininess = m.shininess.toFloat.toDouble,
          texture = textureToFloatToDouble(m.texture))
      }
      def meshToFloatToDouble = (m: MSHMesh) => {
        MSHMesh(
          materials = mshMesh.materials.map { m => materialToFloatToDouble(m) },
          vertex = mshMesh.vertex.map { v => pointToFloatToDouble(v) },
          normal = mshMesh.normal.map { n => vectorToFloatToDouble(n) },
          textureCoordinates = mshMesh.textureCoordinates.map { c => pointToFloatToDouble(c) },
          color = mshMesh.color.map { c => rgbaToFloatToDouble(c) },
          tvi = mshRead.tvi,
          tni = mshMesh.tni,
          tti = mshMesh.tti,
          tci = mshMesh.tci,
          tmi = mshMesh.tmi,
          lvi = mshMesh.lvi,
          lti = mshMesh.lti,
          lci = mshMesh.lci,
          pvi = mshMesh.pvi,
          pci = mshMesh.pci,
          path = mshMesh.path )
      }

      val readMesh = mshRead.copy(path = new File(""))
      readMesh shouldBe meshToFloatToDouble(mshMesh) // NOTE: write and read reduces the precision to floats
    }

    f.delete()
  }
}
