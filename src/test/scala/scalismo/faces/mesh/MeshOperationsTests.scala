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
import scalismo.common.PointId
import scalismo.faces.FacesTestSuite
import scalismo.geometry.{EuclideanVector3D, Point, _3D}
import scalismo.mesh._
import scalismo.faces.mesh.MeshOperations

import scala.collection.immutable.IndexedSeq

class MeshOperationsTests extends FacesTestSuite {

  // deterministic grid mesh, compact and clean
  val basicMesh = randomGridMesh(3, 3, 0.0)

  // tests if a mesh is compact
  def isCompact(mesh: TriangleMesh[_3D]): Boolean = {
    val n = mesh.pointSet.numberOfPoints
    def pointValid(pointId: PointId): Boolean = pointId.id >= 0 && pointId.id < n

    mesh.triangulation.triangles.forall{_.pointIds.forall{pointValid}} &&
    n == mesh.triangulation.pointIds.length
  }

  describe("MeshOperations") {

    describe("compactMesh") {
      describe("applied to a compact mesh") {
        val basicCompact = MeshOperations.compactMesh(basicMesh.shape)

        it("does not alter the mesh") {
          basicCompact.transformedMesh shouldBe basicMesh.shape
        }

        it("does not alter its surface property type (SurfacePointProperty)") {
          val compactColor = basicCompact.applyToSurfaceProperty(basicMesh.color)
          compactColor shouldBe a [SurfacePointProperty[_]]
        }

        it("does not alter its surface property") {
          val compactColor = basicCompact.applyToSurfaceProperty(basicMesh.color)
          val pointColor = SurfacePointProperty.sampleSurfaceProperty[RGBA](compactColor, { seq => seq.head })
          pointColor shouldBe basicMesh.color
        }

        it("the transformed mesh is compact") {
          assert(isCompact(basicCompact.transformedMesh))
        }
      }

      describe("applied to a mesh with invalid triangles") {
        val meshWithInvalidTriangles: TriangleMesh[_3D] = {
          val n = basicMesh.shape.pointSet.numberOfPoints
          val invalidTriangles = IndexedSeq(
            TriangleCell(PointId(-1), PointId(0), PointId(1)),
            TriangleCell(PointId(0), PointId(-1), PointId(1)),
            TriangleCell(PointId(0), PointId(1), PointId(-1)),
            TriangleCell(PointId(n), PointId(0), PointId(1)),
            TriangleCell(PointId(n + 1), PointId(0), PointId(1))
          )
          val allTriangles = TriangleList(invalidTriangles ++ basicMesh.shape.triangulation.triangles)
          TriangleMesh3D(basicMesh.shape.pointSet.points.toIndexedSeq, allTriangles)
        }

        val compactMesh = MeshOperations.compactMesh(meshWithInvalidTriangles)
        val invalidColored = basicMesh.color.copy(triangulation = meshWithInvalidTriangles.triangulation)

        it("the test mesh is not compact") {
            assert(!isCompact(meshWithInvalidTriangles))
        }

        it("removes invalid triangles") {
          compactMesh.transformedMesh.triangulation shouldBe basicMesh.shape.triangulation
        }

        it("transforms a surface property to match new triangle layout") {
          val ptColor = compactMesh.applyToSurfaceProperty(invalidColored).asInstanceOf[SurfacePointProperty[RGBA]]
          ptColor shouldBe basicMesh.color
        }

        it("the transformed mesh is compact") {
          assert(isCompact(compactMesh.transformedMesh))
        }
      }

      describe("applied to a mesh with unreferenced points") {
        val meshWithUnreferencedPoints: VertexColorMesh3D = {
          val newPoints = IndexedSeq.fill(5){randomVector3D.toPoint}
          val newColors = IndexedSeq.fill(5){randomRGBA}

          def up(pointId: PointId): PointId = PointId(pointId.id + 5)
          val trList = TriangleList(
            basicMesh.shape.triangulation.triangles.map{case TriangleCell(p1, p2, p3) => TriangleCell(up(p1), up(p2), up(p3))}
          )

          VertexColorMesh3D(
            shape = TriangleMesh3D(newPoints ++ basicMesh.shape.pointSet.points.toIndexedSeq, trList),
            color = SurfacePointProperty(trList, newColors ++ basicMesh.color.pointData))
        }

        val compactMesh = MeshOperations.compactMesh(meshWithUnreferencedPoints.shape)

        it("the test mesh is not compact") {
          assert(!isCompact(meshWithUnreferencedPoints.shape))
        }

        it("removes unreferenced points") {
          compactMesh.transformedMesh.pointSet shouldBe basicMesh.shape.pointSet
        }

        it("transforms a surface property to match new triangle layout") {
          val compactColor = compactMesh.applyToSurfaceProperty(meshWithUnreferencedPoints.color)
          val ptColor = SurfacePointProperty.sampleSurfaceProperty[RGBA](compactColor, {_.head})
          ptColor shouldBe basicMesh.color
        }

        it("the transformed mesh is compact") {
          assert(isCompact(compactMesh.transformedMesh))
        }
      }
    }

    describe("maskMesh") {
      // remove all left of 0.1
      def removeLeft(pointId: PointId): Boolean = {
        val p = basicMesh.shape.pointSet.point(pointId)
        p.x > 0.1
      }

      val masker = MeshOperations.maskPoints(basicMesh.shape, removeLeft)

      it("actually removes points") {
        masker.transformedMesh.pointSet.numberOfPoints should be < basicMesh.shape.pointSet.numberOfPoints
      }

      it("removes appropriate points and keeps good points") {
        val goodPoints: IndexedSeq[Point[_3D]] = basicMesh.shape.pointSet.pointIds.filter(removeLeft).map(basicMesh.shape.pointSet.point).toIndexedSeq
        val keptPoints: IndexedSeq[Point[_3D]] = masker.transformedMesh.pointSet.points.toIndexedSeq
        keptPoints should contain theSameElementsAs goodPoints
      }

      it("the transformed mesh is compact (removed invalid triangles)") {
        assert(isCompact(masker.transformedMesh))
      }

      it("transforms a point property accordingly") {
        val goodColors = basicMesh.shape.pointSet.pointIds.filter(removeLeft).map(basicMesh.color.atPoint).toIndexedSeq
        val trafoColor = masker.applyToSurfaceProperty(basicMesh.color)
        val maskColor = SurfacePointProperty.sampleSurfaceProperty[RGBA](trafoColor, {_.head})
        maskColor.pointData shouldBe goodColors
      }
    }

    describe("clipMesh") {
      // clip with plane which cuts diagonally from top left to bottom right
      val p0 = Point(0.5, 0.5, 0.0)
      val n = EuclideanVector3D(0.5, 0.5, 0.0).normalize

      val clipper = MeshOperations.clipMeshPoints(basicMesh.shape, p0, n)

      it("actually removes points") {
        clipper.transformedMesh.pointSet.numberOfPoints should be < basicMesh.shape.pointSet.numberOfPoints
      }

      it("removes appropriate points and keeps good points") {
        val goodPoints: IndexedSeq[Point[_3D]] = basicMesh.shape.pointSet.points.filter{ p => (p - p0).dot(n) >= 0}.toIndexedSeq
        val keptPoints: IndexedSeq[Point[_3D]] = clipper.transformedMesh.pointSet.points.toIndexedSeq
        keptPoints should contain theSameElementsAs goodPoints
      }

      it("the transformed mesh is compact (removed invalid triangles)") {
        assert(isCompact(clipper.transformedMesh))
      }

      it("transforms a point property accordingly") {
        val goodPoints = basicMesh.shape.pointSet.points.filter{p => (p - p0).dot(n) >= 0}.toIndexedSeq
        val goodIds: IndexedSeq[PointId] = goodPoints.map{p => basicMesh.shape.pointSet.pointId(p).get}
        val goodColors = goodIds.map(basicMesh.color.atPoint)
        val trafoColor = clipper.applyToSurfaceProperty(basicMesh.color)
        val maskColor = SurfacePointProperty.sampleSurfaceProperty[RGBA](trafoColor, {_.head})
        maskColor.pointData shouldBe goodColors
      }
    }
  }
}
