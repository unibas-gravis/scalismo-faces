/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.image

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.image.PixelImageConversion.BufferedImageConverter

import scala.util.{Failure, Try}

object PixelImageConversion {

  implicit def bufferedImageConverterDouble(implicit doubleConv: BufferedImageConverter[Double]) = new BufferedImageConverter[Double] {
    override def toBufferedImage(image: PixelImage[Double]): BufferedImage = doubleConv.toBufferedImage(image)

    override def fromBufferedImage(image: BufferedImage): PixelImage[Double] = doubleConv.fromBufferedImage(image)
  }

  trait BufferedImageConverter[Pixel] {
    def toBufferedImage(image: PixelImage[Pixel]): BufferedImage
    def fromBufferedImage(image: BufferedImage): PixelImage[Pixel]
  }

  implicit object BufferedImageConverterRGBA extends BufferedImageConverter[RGBA] {
    override def toBufferedImage(image: PixelImage[RGBA]): BufferedImage = {

      def readIndexed8bitColor(x: Int, y: Int): Int = {
        def toInt(d: Double): Int = (d * 255.0).toInt
        val c = image(x, y).clamped
        val intColor = new java.awt.Color(toInt(c.r), toInt(c.g), toInt(c.b), toInt(c.a))
        intColor.getRGB
      }

      // construct BufferedImage
      val bufImg = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_ARGB)
      for (
        x <- 0 until image.width;
        y <- 0 until image.height
      ) {
        bufImg.setRGB(x, y, readIndexed8bitColor(x, y))
      }
      bufImg
    }

    override def fromBufferedImage(image: BufferedImage): PixelImage[RGBA] = {
      val w = image.getWidth
      val h = image.getHeight

      val buffer = ImageBuffer.makeInitializedBuffer(w, h)(RGBA.BlackTransparent)

      // not super fast but ensures a correct mapping between location and pointId, works for col and row major storage
      def readIndexedColor(x: Int, y: Int): RGBA = {
        def toD(i: Int): Double = i / 255.0
        val c: Color = new Color(image.getRGB(x, y), true)
        RGBA(toD(c.getRed), toD(c.getGreen), toD(c.getBlue), toD(c.getAlpha))
      }
      for (
        x <- 0 until w;
        y <- 0 until h
      ) buffer(x, y) = readIndexedColor(x, y)
      buffer.toImage
    }
  }

  implicit object BufferedImageConverterRGB extends BufferedImageConverter[RGB] {
    override def toBufferedImage(image: PixelImage[RGB]): BufferedImage = {

      def readIndexed8bitColor(x: Int, y: Int): Int = {
        def toInt(d: Double): Int = (d * 255.0).toInt
        val c = image(x, y).clamped
        val intColor = new java.awt.Color(toInt(c.r), toInt(c.g), toInt(c.b))
        intColor.getRGB
      }

      // construct BufferedImage
      val bufImg = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_RGB)
      for (
        x <- 0 until image.width;
        y <- 0 until image.height
      ) {
        bufImg.setRGB(x, y, readIndexed8bitColor(x, y))
      }
      bufImg
    }

    override def fromBufferedImage(image: BufferedImage): PixelImage[RGB] = {
      val w = image.getWidth
      val h = image.getHeight

      val buffer = ImageBuffer.makeInitializedBuffer(w, h)(RGB.Black)

      // not super fast but ensures a correct mapping between location and pointId, works for col and row major storage
      def readIndexedColor(x: Int, y: Int): RGB = {
        def toD(i: Int): Double = i / 255.0
        val c: Color = new Color(image.getRGB(x, y))
        RGB(toD(c.getRed), toD(c.getGreen), toD(c.getBlue))
      }
      for (
        x <- 0 until w;
        y <- 0 until h
      ) buffer(x, y) = readIndexedColor(x, y)
      buffer.toImage
    }
  }

  implicit object BufferedImageConverterDouble extends BufferedImageConverter[Double] {
    override def toBufferedImage(image: PixelImage[Double]): BufferedImage = {

      def readIndexed8bitColor(x: Int, y: Int): Int = {
        def toInt(f: Double): Int = (math.min(1.0, math.max(0.0, f)) * 255.0).toInt
        val g = toInt(image(x, y))
        g
      }

      // construct BufferedImage
      val bufImg = new BufferedImage(image.width, image.height, BufferedImage.TYPE_BYTE_GRAY)
      val bufRaster = bufImg.getRaster // write to raster, high-level is only RGB
      for (
        x <- 0 until image.width;
        y <- 0 until image.height
      ) bufRaster.setSample(x, y, 0, readIndexed8bitColor(x, y))
      bufImg
    }

    override def fromBufferedImage(image: BufferedImage): PixelImage[Double] = {
      val w = image.getWidth
      val h = image.getHeight

      val buffer = ImageBuffer.makeInitializedBuffer(w, h)(0.0)

      // not super fast but ensures a correct mapping between location and pointId, works for col and row major storage
      def readIndexedColor(x: Int, y: Int): Double = {
        def toF(i: Int): Double = i / 255.0
        // raw access: circumvent implicit color management (in set/getRGB)
        val g: Int = image.getRaster.getSample(x, y, 0)
        toF(g)
      }
      for (
        x <- 0 until w;
        y <- 0 until h
      ) buffer(x, y) = readIndexedColor(x, y)
      buffer.toImage
    }
  }
}

object PixelImageIO {

  def read[Pixel](file: File)(implicit converter: BufferedImageConverter[Pixel]): Try[PixelImage[Pixel]] = {
    val img: Try[java.awt.image.BufferedImage] = Try(javax.imageio.ImageIO.read(file))
    img.map(converter.fromBufferedImage)
  }

  def write[Pixel](image: PixelImage[Pixel], file: File)(implicit converter: BufferedImageConverter[Pixel]): Try[Unit] = {
    val bufImage = converter.toBufferedImage(image)
    file match {
      case f if f.getAbsolutePath.toLowerCase.endsWith(".png") => Try(javax.imageio.ImageIO.write(bufImage, "png", file))
      case f if f.getAbsolutePath.toLowerCase.endsWith(".jpg") => Try(javax.imageio.ImageIO.write(bufImage, "jpg", file))
      case _ => Failure(new Exception("Unknown image format: " + file.getName))
    }
  }

}
