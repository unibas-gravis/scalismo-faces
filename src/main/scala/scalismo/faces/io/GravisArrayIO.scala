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

import java.io._

import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.utils.ResourceManagement

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Success, Try}

/** read and write for legacy gravis formats */
object GravisArrayIO {

  /** read default human readable array format */
  def read[A: ClassTag](file: File, sizeHint: Int = 100)(implicit reader: GravisTypeIO[A]): Try[IndexedSeq[A]] = {
    readFromSource[A](Source.fromFile(file), sizeHint)
  }

  /** read default human readable array format from Source */
  def readFromSource[A: ClassTag](source: Source, sizeHint: Int = 100)(implicit reader: GravisTypeIO[A]): Try[IndexedSeq[A]] = {
    readHumanReadableFromSource(source, sizeHint)
  }

  /** read array in standard human readable format from legacy gravis: "[index]=value" or plain list format with a value per line */
  def readHumanReadable[A: ClassTag](file: File, sizeHint: Int = 100)(implicit reader: GravisTypeIO[A]): Try[IndexedSeq[A]] = {
    readHumanReadableFromSource(Source.fromFile(file), sizeHint)
  }

  /** read array in standard human readable format from legacy gravis: [index]=value or plain list format with a value per line */
  def readHumanReadableFromSource[A: ClassTag](source: Source, sizeHint: Int = 100)(implicit reader: GravisTypeIO[A]): Try[IndexedSeq[A]] = {
    val buffer = new ArrayBuffer[A](/*source.getLines().size*/ sizeHint)
    val lines = source.getLines().map(_.trim)
    val indexRE = """^\[([0-9]+)\]=(.*)$""".r("index", "value")
    var index: Int = 0
    for (l <- lines if l.nonEmpty) {
      val (ind, valString) = l.trim match {
        case indexRE(i, value) => (i.toInt, value)
        case value: String => (index, value)
      }
      // fill empty indices
      while (index < ind) {
        buffer += reader.empty
        index += 1
      }
      // parse value and add accordingly
      buffer += reader.parse(valString)
      index += 1
    }
    Success(buffer.toIndexedSeq)
  }

  /** write array in default format: plain readable list */
  def write[A](data: Iterable[A], file: File)(implicit io: GravisTypeIO[A]): Try[Unit] = writeList(data, file)

  /** write a human readable list: 1 value per line */
  def writeList[A](data: Iterable[A], file: File)(implicit io: GravisTypeIO[A]): Try[Unit] = {
    ResourceManagement.using(new PrintWriter(new FileOutputStream(file))) { w =>
      data.foreach(d => w.println(io.toString(d)))
    }
    Success(Unit)
  }

  /** write human readable legacy gravis format: [index]=value (Sandro) */
  def writeLegacyHumanReadable[A](data: Iterable[A], file: File)(implicit io: GravisTypeIO[A]): Try[Unit] = {
    val lines = data.iterator.zipWithIndex.map{ case (d, i) => s"[$i]=${io.toString(d)}"}
    ResourceManagement.using(new PrintWriter(new FileOutputStream(file))) { w =>
      lines.foreach(w.println)
    }
    Success(Unit)
  }

  /** typed gravis reader/writer, String parser and binary - for legacy gravis formats */
  trait GravisTypeIO[A] {
    /** parse value from String */
    def parse(string: String): A

    /** empty default value */
    def empty: A

    /** write to String */
    def toString(data: A): String
  }

  /** read and write Int in gravis-compatible format */
  implicit object IntIO extends GravisTypeIO[Int] {
    override def empty: Int = 0

    override def parse(string: String): Int = string.toInt

    override def toString(data: Int): String = data.toString
  }

  /** read and write Long in gravis-compatible format */
  implicit object LongIO extends GravisTypeIO[Long] {
    override def empty: Long = 0

    override def parse(string: String): Long = string.toLong

    override def toString(data: Long): String = data.toString
  }

  /** read and write Float in gravis-compatible format */
  implicit object FloatIO extends GravisTypeIO[Float] {
    override def empty: Float = 0.0f

    override def parse(string: String): Float = string.toFloat

    override def toString(data: Float): String = "%.10f".formatLocal(java.util.Locale.US, data)
  }

  /** read and write Double in gravis-compatible format */
  implicit object DoubleIO extends GravisTypeIO[Double] {
    override def empty: Double = 0.0

    override def parse(string: String): Double = string.toDouble

    override def toString(data: Double): String = "%.20f".formatLocal(java.util.Locale.US, data)
  }

  /** read and write Tuple3 (Int, Int, Int) in gravis-compatible format */
  implicit object Tuple3IO extends GravisTypeIO[(Int, Int, Int)] {
    override def empty: (Int, Int, Int) = (0, 0, 0)

    override def parse(string: String): (Int, Int, Int) = {
      val values = string.trim.replaceAll("\\[", "").replaceAll("\\]", "").split(",").map(_.trim.toInt)
      (values(0), values(1), values(2))
    }

    override def toString(data: (Int, Int, Int)): String = s"[${data._1}, ${data._2}, ${data._3}]"
  }

  /** read and write RGB in gravis-compatible format */
  implicit object RGBIO extends GravisTypeIO[RGB] {
    override def empty: RGB = RGB(0, 0, 0)

    override def parse(string: String): RGB = {
      val values = string.trim.replaceAll("\\(", "").replaceAll("\\)", "").split(",").map(_.trim.toDouble)
      RGB(values(0), values(1), values(2))
    }

    override def toString(data: RGB): String = Seq( data.r, data.g, data.b).map(v => "%.20f".formatLocal(java.util.Locale.US,v)).mkString(", ")
  }

  /** read and write RGBA in gravis-compatible format */
  implicit object RGBAIO extends GravisTypeIO[RGBA] {
    override def empty: RGBA = RGBA(0, 0, 0, 1)

    override def parse(string: String): RGBA = {
      val values = string.trim.replaceAll("\\(", "").replaceAll("\\)", "").split(",").map(_.trim.toDouble)
      RGBA(values(0), values(1), values(2), values(3))
    }

    override def toString(data: RGBA): String = Seq( data.r, data.g, data.b, data.a).map(v => "%.20f".formatLocal(java.util.Locale.US,v)).mkString(", ")
  }

}
