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

package scalismo.faces.landmarks

import java.io._

import scalismo.faces.FacesTestSuite
import scalismo.faces.io.TLMSLandmarksIO
import scalismo.faces.utils.ResourceManagement
import scalismo.geometry.{Point, Point2D, Point3D}

import scala.io.Source

class LandmarksTest extends FacesTestSuite {

  def createRandom2DLandmarks(n: Int): IndexedSeq[TLMSLandmark2D] = {
    for (i <- 0 until n) yield TLMSLandmark2D(randomString(rnd.scalaRandom.nextInt(10) + 1), new Point2D(rnd.scalaRandom.nextDouble, rnd.scalaRandom.nextDouble), rnd.scalaRandom.nextBoolean())
  }

  def createRandom3DLandmarks(n: Int): IndexedSeq[TLMSLandmark3D] = {
    for (i <- 0 until n) yield TLMSLandmark3D(randomString(rnd.scalaRandom.nextInt(10) + 1), new Point3D(rnd.scalaRandom.nextDouble, rnd.scalaRandom.nextDouble, rnd.scalaRandom.nextDouble), rnd.scalaRandom.nextBoolean())
  }

  describe("Landmarks 2D") {
    val lms = createRandom2DLandmarks(25)

    it("can write and read from file (enforce TLMS Float format)") {
      val tmpFile = File.createTempFile("tlms2d", ".tlms")
      tmpFile.deleteOnExit()
      TLMSLandmarksIO.write2D(lms, tmpFile).get
      val readLM = TLMSLandmarksIO.read2D(tmpFile).get
      // cast landmarks to float
      val floatLM = lms.map{lm => lm.copy(point = Point(lm.point.x.toFloat, lm.point.y.toFloat))}
      // should not write/read as double
      readLM should not be lms
      // but as float
      readLM shouldBe floatLM
    }

    it("can write to an existing output stream without closing it") {
      val f = File.createTempFile("tlms2d", ".txt")
      f.deleteOnExit()
      val oStream = new FileOutputStream(f)
      TLMSLandmarksIO.write2DToStream(lms, oStream).get
      ResourceManagement.using(new PrintWriter(oStream)) { writer =>
        writer.println("stream should still accept more text")
      }
      Source.fromFile(f).getLines().length should be (lms.length + 1)
    }

    it("can be converted to Landmarks") {
      val scLMs = lms.map(lm => lm.toLandmark)
      lms.zip(scLMs).foreach{ case(tlm, scLM) =>
        scLM.id shouldBe tlm.id
        scLM.point shouldBe tlm.point
      }
    }
  }

  describe("Landmarks 3D") {
    val lms = createRandom3DLandmarks(25)

    it("can write / read from file (and properly convert to Float thereby, TLMS is float)") {
      val tmpFile = File.createTempFile("tlms3d",".tlms")
      tmpFile.deleteOnExit()
      TLMSLandmarksIO.write3D(lms, tmpFile).get
      val readLM = TLMSLandmarksIO.read3D(tmpFile).get
      val floatLM = lms.map{lm => lm.copy(point = Point(lm.point.x.toFloat, lm.point.y.toFloat, lm.point.z.toFloat))}
      // should not write/read as double
      readLM should not be lms
      // but as float
      readLM shouldBe floatLM
    }

    it("can write to an existing output stream without closing it") {
      val f = File.createTempFile("tlms3d", ".txt")
      f.deleteOnExit()
      val oStream = new FileOutputStream(f)
      TLMSLandmarksIO.write3DToStream(lms, oStream).get
      ResourceManagement.using(new PrintWriter(oStream)) { writer =>
        writer.println("stream should still accept more text")
      }
      Source.fromFile(f).getLines().length should be (lms.length + 1)
    }

    it("can be converted to Landmarks") {
      val scLMs = lms.map(lm => lm.toLandmark)
      lms.zip(scLMs).foreach{ case(tlm, scLM) =>
        scLM.id shouldBe tlm.id
        scLM.point shouldBe tlm.point
      }
    }
  }
}
