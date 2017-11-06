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

import scalismo.faces.landmarks.{TLMSLandmark2D, TLMSLandmark3D}
import scalismo.faces.utils.ResourceManagement
import scalismo.geometry._

import scala.io.Source
import scala.util.Try

object TLMSLandmarksIO {
  /** read TLMS 2D format from a file (format: "id visible x y")*/
  def read2D(file: File): Try[IndexedSeq[TLMSLandmark2D]] = {
    ResourceManagement.usingTry(Try(new FileInputStream(file)))(read2DFromStream)
  }

  /** read TLMS 2D format from a stream (format: "id visible x y") */
  def read2DFromStream(stream: InputStream): Try[IndexedSeq[TLMSLandmark2D]] = Try {
    val lines = Source.fromInputStream(stream).getLines().filter(!_.isEmpty)
    lines.map { line =>
      val fields = line.split("\\s+").map(_.trim)
      require(fields.length == 4, "landmark file not in correct format, expects name, visibility (0 or 1), x-coordinate, y-coordinate per line")
      val name = fields(0)
      val visibility: Boolean = fields(1).toInt > 0
      val x = fields(2).toFloat
      val y = fields(3).toFloat
      TLMSLandmark2D(name, Point(x, y), visibility)
    }.toIndexedSeq
  }

  /** read TLMS 3D format from a file (format: "id visible x y z") */
  def read3D(file: File): Try[IndexedSeq[TLMSLandmark3D]] = {
    ResourceManagement.usingTry(Try(new FileInputStream(file)))(read3DFromStream)
  }

  /** read TLMS 3D format from a stream (format: "id visible x y z") */
  def read3DFromStream(stream: InputStream): Try[IndexedSeq[TLMSLandmark3D]] = Try {
    val lines = Source.fromInputStream(stream).getLines().filter(!_.isEmpty)
    lines.map { line =>
      val fields = line.split("\\s+").map(_.trim)
      require(fields.length == 5, "landmark file not in correct format, expects name, visibility (0 or 1), x-coordinate, y-coordinate, z-coordinate per line")
      val name = fields(0)
      val visibility: Boolean = fields(1).toInt > 0
      val x = fields(2).toFloat
      val y = fields(3).toFloat
      val z = fields(4).toFloat
      TLMSLandmark3D(name, Point(x, y, z), visibility)
    }.toIndexedSeq
  }

  /** write TLMS 2D landmarks format to a stream (format: "id visible x y") */
  def write2DToStream(landmarks: IndexedSeq[TLMSLandmark2D], stream: OutputStream): Try[Unit] = Try {
    ResourceManagement.using(new PrintWriter(stream), (wr: PrintWriter) => wr.flush()) { writer =>
      landmarks.foreach{lm =>
        val visible = if (lm.visible) "1" else "0"
        val line = f"${lm.id}%s $visible%s ${lm.point.x}%.17g ${lm.point.y}%.17g"
        writer.println(line)
      }
    }
  }

  /** write TLMS 2D landmarks format to a file (format: "id visible x y") */
  def write2D(landmarks: IndexedSeq[TLMSLandmark2D], file: File): Try[Unit] = Try {
    ResourceManagement.usingTry(Try(new FileOutputStream(file)))(stream => write2DToStream(landmarks, stream))
  }

  /** write TLMS 2D landmarks format to a stream (format: "id visible x y z") */
  def write3DToStream(landmarks: IndexedSeq[TLMSLandmark3D], stream: OutputStream): Try[Unit] = Try {
    ResourceManagement.using(new PrintWriter(stream), (wr: PrintWriter) => wr.flush()) { writer =>
      landmarks.foreach { lm =>
        val visible = if (lm.visible) "1" else "0"
        writer.println(f"${lm.id}%s $visible%s ${lm.point.x}%.17g ${lm.point.y}%.17g ${lm.point.z}%.17g")
      }
    }
  }

  /** write TLMS 2D landmarks format to a file (format: "id visible x y z") */
  def write3D(landmarks: IndexedSeq[TLMSLandmark3D], file: File): Try[Unit] = {
    ResourceManagement.usingTry(Try(new FileOutputStream(file)))(stream => write3DToStream(landmarks, stream))
  }

}
