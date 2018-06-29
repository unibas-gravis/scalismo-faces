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
import scalismo.faces.mesh.{ColorNormalMesh3D, OptionalColorNormalMesh3D, TextureMappedProperty, VertexColorMesh3D}
import scalismo.geometry.{Point, _2D}
import scalismo.mesh.{BarycentricCoordinates, SurfacePointProperty, TriangleId}

class MeshIOTests extends FacesTestSuite {

  private def testDiffTexture(tex1: TextureMappedProperty[RGBA], tex2: TextureMappedProperty[RGBA]): Unit = {
    import scalismo.faces.image.PixelImage

    // triangulation
    tex1.triangulation shouldBe tex2.triangulation

    // texture image
    import PixelImage.implicits._
    val texDiff = math.sqrt((tex1.texture - tex2.texture).normSq / tex1.texture.length)

    // color difference of texture image should be small
    texDiff should be < 0.01

    val sptm1 = SurfacePointProperty.sampleSurfaceProperty[Point[_2D]](tex1.textureMapping, {
      _.head
    })
    val sptm2 = SurfacePointProperty.sampleSurfaceProperty[Point[_2D]](tex2.textureMapping, {
      _.head
    })
    val tmMaxDiff = math.sqrt(sptm1.pointData.zip(sptm2.pointData).map { case (p1, p2) => (p1 - p2).norm2 }.max)

    // difference in texture coordinates should be small
    tmMaxDiff should be < 0.01

    // color difference lookup should be small
    // random sample points on surface to determine color difference
    val surfacePoints = IndexedSeq.fill(10)((TriangleId(randomInt(tex1.triangulation.triangles.length)), BarycentricCoordinates.randomUniform))
    val colorValues1 = surfacePoints.map { case (tId, bcc) => tex1.onSurface(tId, bcc) }
    val colorValues2 = surfacePoints.map { case (tId, bcc) => tex2.onSurface(tId, bcc) }

    val maxColorDiff = math.sqrt(colorValues1.zip(colorValues2).map {
      case (read, target) =>
        val d = read - target
        d.dot(d)
    }.max)

    // color difference should be small
    maxColorDiff should be < 0.01
  }

  private def testDiffOptionalColorNormalMesh3D(mesh1: OptionalColorNormalMesh3D, mesh2: OptionalColorNormalMesh3D): Unit = {
    // ensure same parts are there
    mesh1.color.isDefined shouldBe mesh2.color.isDefined
    mesh1.normals.isDefined shouldBe mesh2.normals.isDefined

    // mesh geometry configuration should be identical
    mesh1.shape.triangulation shouldBe mesh2.shape.triangulation
    mesh1.shape.pointSet.numberOfPoints shouldBe mesh2.shape.pointSet.numberOfPoints

    // shape difference
    val maxPointDiff = math.sqrt(mesh1.shape.pointSet.points.zip(mesh2.shape.pointSet.points).map {
      case (read, target) => (read - target).norm2
    }.max)

    maxPointDiff should be < 0.01

    // color difference
    for (col1 <- mesh1.color; col2 <- mesh2.color) {
      // random sample points on surface to determine color difference
      val surfacePoints = IndexedSeq.fill(10)((TriangleId(randomInt(mesh1.shape.triangulation.triangles.length)), BarycentricCoordinates.randomUniform))
      val colorValues1 = surfacePoints.map { case (tId, bcc) => col1.onSurface(tId, bcc) }
      val colorValues2 = surfacePoints.map { case (tId, bcc) => col2.onSurface(tId, bcc) }

      val maxColorDiff = math.sqrt(colorValues1.zip(colorValues2).map {
        case (read, target) =>
          val d = read - target
          d.dot(d)
      }.max)

      // color difference should be small
      maxColorDiff should be < 0.01
    }

    // normal difference
    for (norm1 <- mesh1.normals; norm2 <- mesh2.normals) {
      val surfacePoints = IndexedSeq.fill(10)((TriangleId(randomInt(mesh1.shape.triangulation.triangles.length)), BarycentricCoordinates.randomUniform))
      val normalValues1 = surfacePoints.map { case (tId, bcc) => norm1.onSurface(tId, bcc) }
      val normalValues2 = surfacePoints.map { case (tId, bcc) => norm2.onSurface(tId, bcc) }
      val maxNormalDiff = math.sqrt(normalValues1.zip(normalValues2).map {
        case (read, target) => (read - target).norm2
      }.max)

      // normals should be similar
      maxNormalDiff should be < 0.01
    }
  }

  private def testWriteReadCycleVertexColor(mesh: VertexColorMesh3D, f: File): Unit = {
    MeshIO.write(mesh, f).get
    val readMesh = MeshIO.read(f).get
    val vcReadMesh = readMesh.toVertexColorMesh3D
    assert(vcReadMesh.isDefined)
    testDiffOptionalColorNormalMesh3D(
      OptionalColorNormalMesh3D.fromVertexColorMesh(vcReadMesh.get),
      OptionalColorNormalMesh3D.fromVertexColorMesh(mesh)
    )
  }

  private def testWriteReadCycleWithTexture(mesh: ColorNormalMesh3D, f: File): Unit = {
    MeshIO.write(mesh, f).get
    val readMesh = MeshIO.read(f).get
    assert(readMesh.hasColor)
    assert(readMesh.hasTexture)
    assert(readMesh.hasNormals)
    val vcReadMesh = readMesh.colorNormalMesh3D
    assert(vcReadMesh.isDefined)
    testDiffTexture(
      vcReadMesh.get.color.asInstanceOf[TextureMappedProperty[RGBA]],
      mesh.color.asInstanceOf[TextureMappedProperty[RGBA]])
    testDiffOptionalColorNormalMesh3D(
      OptionalColorNormalMesh3D.fromColorNormalMesh(vcReadMesh.get),
      OptionalColorNormalMesh3D.fromColorNormalMesh(mesh))
  }

  private def testWriteReadCyclePLY(mesh: OptionalColorNormalMesh3D): Unit = {
    val f = File.createTempFile("scalismo-test-meshio", ".ply")
    f.deleteOnExit()
    MeshIO.write(mesh, f).get
    val readMesh = MeshIO.read(f).get

    val correctedMesh = if ( readMesh.normals.isDefined && !mesh.normals.isDefined){ // maybe normals are generated while reading
      OptionalColorNormalMesh3D(readMesh.shape,readMesh.color,None)
    } else readMesh

    testDiffOptionalColorNormalMesh3D(
      correctedMesh,
      mesh
    )
  }

  describe("MeshIO") {

    describe("can write and read a random vertex color mesh") {
      val rndMesh = randomGridMesh()

      it("with ply format") {
        val f = File.createTempFile("scalismo-faces-test-meshio", ".ply")
        f.deleteOnExit()
        testWriteReadCycleVertexColor(rndMesh, f)
      }

      it("with msh format") {
        val f = File.createTempFile("scalismo-faces-test-meshio", ".msh.gz")
        f.deleteOnExit()
        testWriteReadCycleVertexColor(rndMesh, f)
      }
    }

    describe("can write and read a random texture color mesh") {
      val rndMesh: ColorNormalMesh3D = randomGridMeshWithTexture(25, 25)

      it("check: mesh is similar to itself") {
        testDiffOptionalColorNormalMesh3D(
          OptionalColorNormalMesh3D.fromColorNormalMesh(rndMesh),
          OptionalColorNormalMesh3D.fromColorNormalMesh(rndMesh))
      }

      it("with ply format") {
        val f = File.createTempFile("scalismo-faces-test-meshio", ".ply")
        f.deleteOnExit()
        testWriteReadCycleWithTexture(rndMesh, f)
      }

      it("with msh format") {
        val f = File.createTempFile("scalismo-faces-test-meshio", ".msh.gz")
        f.deleteOnExit()
        testWriteReadCycleWithTexture(rndMesh, f)
      }
    }

    describe("can deal with various mesh complexities in ply format") {
      val rndMeshTex: ColorNormalMesh3D = randomGridMeshWithTexture(5, 5)
      val rndMesh: ColorNormalMesh3D = ColorNormalMesh3D(VertexColorMesh3D(rndMeshTex.shape, SurfacePointProperty.averagedPointProperty(rndMeshTex.color)))

      it("a pure shape mesh") {
        val mesh = OptionalColorNormalMesh3D(rndMeshTex.shape, None, None)
        testWriteReadCyclePLY(mesh)
      }

      it("a mesh with vertex color and normals") {
        val mesh = OptionalColorNormalMesh3D(rndMeshTex.shape, Some(rndMesh.color), Some(rndMesh.normals))
        testWriteReadCyclePLY(mesh)
      }

      it("a mesh with texture and normals") {
        val mesh = OptionalColorNormalMesh3D(rndMeshTex.shape, Some(rndMeshTex.color), Some(rndMesh.normals))
        testWriteReadCyclePLY(mesh)
      }

      it("a vertex color mesh without normals") {
        val mesh = OptionalColorNormalMesh3D(rndMesh.shape, Some(rndMesh.color), None)
        testWriteReadCyclePLY(mesh)
      }
    }
  }
}
