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
import scalismo.color.RGB
import scalismo.faces.io.GravisArrayIO.GravisTypeIO

import scala.reflect.ClassTag

class ArrayIOTest extends FacesTestSuite {

  private def writeReadSeq[A: ClassTag](seq: IndexedSeq[A])(implicit typeio: GravisTypeIO[A]): IndexedSeq[A] = {
    val f = File.createTempFile("scalismo-faces-test-array", ".txt")
    f.deleteOnExit()
    GravisArrayIO.write(seq, f)
    GravisArrayIO.read[A](f).get
  }

  describe("Array IO") {

    it("can write and read a random sequence for A=Double") {
      val seq = IndexedSeq.fill(25)(randomDouble)
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read a random sequence for A=Int") {
      val seq = IndexedSeq.fill(25)(randomInt(Integer.MAX_VALUE))
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read a random sequence for A=Long") {
      val seq = IndexedSeq.fill(25)(rnd.scalaRandom.nextLong())
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read a random sequence for A=Float") {
      val seq = IndexedSeq.fill(25)(rnd.scalaRandom.nextFloat())
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read a random sequence for A=(Int, Int, Int)") {
      val seq =
        IndexedSeq.fill(25)((randomInt(Integer.MAX_VALUE), randomInt(Integer.MAX_VALUE), randomInt(Integer.MAX_VALUE)))
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read a random sequence for A=RGB") {
      val seq = IndexedSeq.fill(25)(randomRGB)
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read a random sequence for A=RGBA") {
      val seq = IndexedSeq.fill(25)(randomRGBA)
      writeReadSeq(seq) should contain theSameElementsInOrderAs seq
    }

    it("can write and read the legacy format ([i]=x) for a random sequence of Ints") {
      val seq = IndexedSeq.fill(25)(randomInt(Integer.MAX_VALUE))
      val f = File.createTempFile("scalismo-faces-test-array-int", ".mask")
      f.deleteOnExit()
      GravisArrayIO.writeLegacyHumanReadable(seq, f)
      val readSeq = GravisArrayIO.read[Int](f).get
      readSeq should contain theSameElementsInOrderAs seq
    }

    it("can write and read the legacy format ([i]=x) for a random sequence of RGB") {
      val seq = IndexedSeq.fill(25)(randomRGB)
      val f = File.createTempFile("scalismo-faces-test-array-rgb", ".color")
      f.deleteOnExit()
      GravisArrayIO.writeLegacyHumanReadable(seq, f)
      val readSeq = GravisArrayIO.read[RGB](f).get
      readSeq should contain theSameElementsInOrderAs seq
    }
  }
}
