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

import java.io.{File, IOException, InputStream, OutputStream}

import scalismo.faces.image.{BufferedImageConverter, PixelImage}

import scala.util.Try


object PixelImageIO {
  /** read image from stream */
  def readFromStream[Pixel](inputStream: InputStream)(implicit converter: BufferedImageConverter[Pixel]): Try[PixelImage[Pixel]] = Try {
    val img = javax.imageio.ImageIO.read(inputStream)
    converter.toPixelImage(img)
  }

  /** write image to a stream, format needs to be specified
    * @param format format suffix, e.g. "png", "jpg" */
  def writeToStream[Pixel](image: PixelImage[Pixel], outputStream: OutputStream, format: String = "png")(implicit converter: BufferedImageConverter[Pixel]): Try[Unit] = Try {
    val bufImage = converter.toBufferedImage(image)
    javax.imageio.ImageIO.write(bufImage, format, outputStream)
  }

  /** read image from a file */
  def read[Pixel](file: File)(implicit converter: BufferedImageConverter[Pixel]): Try[PixelImage[Pixel]] = Try {
    val img = javax.imageio.ImageIO.read(file)
    converter.toPixelImage(img)
  }

  /** write image to a file */
  def write[Pixel](image: PixelImage[Pixel], file: File)(implicit converter: BufferedImageConverter[Pixel]): Try[Unit] = Try {
    val bufImage = converter.toBufferedImage(image)
    file match {
      case f if f.getAbsolutePath.toLowerCase.endsWith(".png") => javax.imageio.ImageIO.write(bufImage, "png", file)
      case f if f.getAbsolutePath.toLowerCase.endsWith(".jpg") => javax.imageio.ImageIO.write(bufImage, "jpg", file)
      case _ => throw new IOException("Unknown image format: " + file.getName)
    }
  }
}
