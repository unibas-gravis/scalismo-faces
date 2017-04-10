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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStreamWriter}
import java.nio.ByteOrder
import java.util.Scanner

import scalismo.faces.FacesTestSuite
import scalismo.faces.io.ply._

class PLYReadWriteTests extends FacesTestSuite {

  describe("Write-read cycles to string, big- and little endian") {

    def testRWEndianCycle[A:StringWriter:StringReader:EndianWriter:EndianReader](toWrite: IndexedSeq[A], bo: ByteOrder): Unit = {
      val N = toWrite.size
      val os = new ByteArrayOutputStream()
      val writer = new SequenceWriter[A]
      writer.write(toWrite, os, bo)

      val ba = os.toByteArray

      val is = new ByteArrayInputStream(ba)
      val reader = new FixedLengthSequenceReader[A]
      val read = reader.read(N, is, bo)

      read.zip(toWrite).foreach { p =>
        p._1 shouldBe p._2
      }
    }

    def testRWStringCycle[A:StringWriter:StringReader:EndianWriter:EndianReader](toWrite: IndexedSeq[A]): Unit = {
      val N = toWrite.size
      val os = new ByteArrayOutputStream()
      val osw = new OutputStreamWriter(os)
      val writer = new SequenceWriter[A]
      writer.write(toWrite, osw)
      osw.flush()

      val is = new ByteArrayInputStream(os.toByteArray)
      val isr = new Scanner(is)
      val reader = new FixedLengthSequenceReader[A]
      val read = reader.read(N, isr)

      read.zip(toWrite).foreach { p =>
        p._1 shouldBe p._2
      }
    }

    def testAllThreeCycles[A:StringWriter:StringReader:EndianWriter:EndianReader](toWrite: IndexedSeq[A]): Unit = {
      testRWStringCycle(toWrite)
      testRWEndianCycle(toWrite, ByteOrder.BIG_ENDIAN)
      testRWEndianCycle(toWrite, ByteOrder.LITTLE_ENDIAN)
    }

    it("should result in the same sequence of bytes") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255).toByte
      testAllThreeCycles(toWrite)
    }
    it("should result in the same sequence of char") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255).toChar
      testAllThreeCycles(toWrite)
    }
    it("should result in the same sequence of short") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255).toShort
      testAllThreeCycles(toWrite)
    }
    it("should result in the same sequence of int") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255).toInt
      testAllThreeCycles(toWrite)
    }
    it("should result in the same sequence of long") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255).toLong
      testAllThreeCycles(toWrite)
    }
    it("should result in the same sequence of float") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255).toFloat
      testAllThreeCycles(toWrite)
    }
    it("should result in the same sequence of double") {
      val toWrite = for (i <- 0 until 20) yield (randomDouble * 255)
      testAllThreeCycles(toWrite)
    }

  }

}
