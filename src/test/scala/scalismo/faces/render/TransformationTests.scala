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

package scalismo.faces.render

import scalismo.faces.FacesTestSuite
import scalismo.color.RGBA
import scalismo.faces.image.{PixelImage, PixelImageOperations}
import scalismo.faces.parameters.RenderParameter
import scalismo.geometry._

class TransformationTests extends FacesTestSuite {
  describe("decompose rotation around unit vectors with random angles") {
    it("decomposes corectly with angles from -pi/2 to pi/2") {
      def randomEulerDifference() = {
        val x = (rnd.scalaRandom.nextDouble() - 0.5) * math.Pi
        val y = (rnd.scalaRandom.nextDouble() - 0.5) * math.Pi
        val z = (rnd.scalaRandom.nextDouble() - 0.5) * math.Pi
        val dec = Rotation3D.decomposeRotationXYZ(Rotation3D.fromEulerXYZ(x, y, z))
        val diff = Vector(x, y, z) - Vector(dec._1, dec._2, dec._3)
        val res = diff.x + diff.y + diff.z
        res
      }
      val differences = IndexedSeq.fill(1000)(randomEulerDifference())
      all(differences) should be < 1e-3
    }

    it("composes correctly with angles from -2pi to 2pi") {
      def randomEulerMatrixDifference(): Double = {
        val x = (rnd.scalaRandom.nextDouble() - 0.5) * 4.0 * math.Pi
        val y = (rnd.scalaRandom.nextDouble() - 0.5) * 4.0 * math.Pi
        val z = (rnd.scalaRandom.nextDouble() - 0.5) * 4.0 * math.Pi
        val rot = Rotation3D.fromEulerXYZ(x, y, z)
        val decrot = Rotation3D.decomposeRotationXYZ(rot)
        val rotA = rot.rotationMatrix
        val rotB = (Rotation3D.fromEulerXYZ _).tupled(decrot).rotationMatrix
        val diff = (0 until 3).zip(0 until 3).map((idx) => rotA(idx._1, idx._2) - rotB(idx._1, idx._2))
        val res = diff.sum
        res
      }
      val differences = IndexedSeq.fill(1000)(randomEulerMatrixDifference())
      all(differences) should be < 1e-3
    }
  }

  def isClose(p: Point[_3D], q: Point[_3D], tolerance: Double = 1e-5): Boolean = (p - q).norm < tolerance

  describe("view lookAt transform") {
    val origin = Point3D.origin
    val x = Point(1.0, 2.0, 3.0)
    val y = Point(1.0, 1.0, 3.0)
    it("centers on correct point") {
      val vT = RenderTransforms.viewTransformLookAt(origin, x, Vector3D.unitY)
      val d = x.toVector.norm
      isClose(vT(x), Point(0.0, 0.0, -d)) shouldBe true
    }

    it("respects upright vector direction") {
      val vT = RenderTransforms.viewTransformLookAt(origin, x.copy(y = 0), -Vector3D.unitY)
      val d = x.toVector.norm
      vT(x).y shouldBe -2.0
      vT(y).y shouldBe -1.0
    }

    it("moves the camera and looks back") {
      val vT = RenderTransforms.viewTransformLookAt(x, origin, Vector3D.unitY)
      val d = x.toVector.norm
      isClose(vT(origin), Point(0.0, 0.0, -d)) shouldBe true
      isClose(vT(x), origin) shouldBe true
    }
  }

  describe("window transform") {

    def within(a: Double, b: Double, eps: Double) = math.abs(a-b) < eps
    def pointWithin(a: Point[_3D], b: Point[_3D], eps: Double) = within(a.x, b.x, eps) && within(a.y, b.y, eps) && within(a.z, b.z, eps)

    it("is inverted correctly") {

      def transformationRoundTripConsistent = {
        val trf = WindowBoxTransform(100, 100, 10, 10, 60, 70)
        val itrf = InverseWindowBoxTransform(100, 100, 10, 10, 60, 70)

        val points = IndexedSeq(Point(-1, -1, -1), Point(0.2, 0.4, -1), Point(0.11, 0.21, -1))
        def testPoint(pt: Point[_3D]) = {
          pointWithin(pt, itrf(trf(pt)), 1e-10)
        }
        val result = testPoint _
        points.forall(result)
      }

      transformationRoundTripConsistent shouldBe true
    }

    it("results in a correct rendering") {

      def renderedImageSameAsExtractedBox() = {
        val param = RenderParameter.defaultSquare
        val mesh = randomGridMesh(100, 100, 0.25)
        val rendering =  {
          val buffer = ZBuffer(param.imageSize.width, param.imageSize.height, RGBA.BlackTransparent)
          TriangleRenderer.renderMesh(mesh.shape, param.pointShader, PixelShaders.PropertyShader(mesh.color), buffer)
          buffer.toImage
        }
        //define box
        val (x, y) = (350, 150)
        val (w, h) = (20, 20)
        val trfScreen = WindowBoxTransform(param.imageSize.width, param.imageSize.height, w, h, x, y)
        val renderingBox =  {
          val buffer = ZBuffer(w, h, RGBA.BlackTransparent)
          TriangleRenderer.renderMesh(mesh.shape, param.pointShader, trfScreen, PixelShaders.PropertyShader(mesh.color), buffer)
          buffer.toImage
        }
        val groundTruth = PixelImageOperations.subImage(rendering, x, y, w, h)

        import PixelImage.implicits._
        val diff = PixelImageOperations.imageNorm(groundTruth - renderingBox)
        diff < 1e-9
      }

      renderedImageSameAsExtractedBox() shouldBe true
    }
  }

}
