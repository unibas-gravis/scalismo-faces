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

import scalismo.faces.color.RGBA
import scalismo.faces.image.{ImageBuffer, PixelImage}

import scala.reflect.ClassTag

/**
  * general structure for a rendering target buffer: stores result and handles multiple fragments per position, this is the renderer's viewport
  * y orientation is upwards! (Coordinate system of an OpenGL viewport)
  */
trait RenderBuffer[A] {
  def update(x: Int, y: Int, z: Double, v: A): Unit

  def isDefinedAt(x: Int, y: Int): Boolean

  def width: Int

  def height: Int

  def toImage: PixelImage[A]
}

/** simple z buffer administrator - standard render buffer for most applications */
case class ZBuffer[A: ClassTag](override val width: Int, override val height: Int, background: A, zInit: Double = Double.PositiveInfinity) extends RenderBuffer[A] {
  private val buffer = ImageBuffer.makeConstantBuffer(width, height, background)
  private val zBuffer = ImageBuffer.makeConstantBuffer(width, height, zInit)

  override def update(x: Int, y: Int, z: Double, v: A): Unit = {
    if (isDefinedAt(x, y)) {
      if (z < zBuffer(x, y)) {
        buffer(x, y) = v
        zBuffer(x, y) = z
      }
    }
  }

  override def toImage: PixelImage[A] = buffer.toImage

  override def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

/** synchronized z buffer for parallel rendering */
case class SyncedZBuffer[A: ClassTag](override val width: Int, override val height: Int, background: A, zInit: Double = Double.PositiveInfinity) extends RenderBuffer[A] {
  private val buffer = ImageBuffer.makeConstantBuffer(width, height, background)
  private val zBuffer = ImageBuffer.makeConstantBuffer(width, height, zInit)

  override def update(x: Int, y: Int, z: Double, v: A): Unit = {
    if (isDefinedAt(x, y)) {
      buffer.synchronized {
        if (z < zBuffer(x, y)) {
          buffer(x, y) = v
          zBuffer(x, y) = z
        }
      }
    }
  }

  override def toImage: PixelImage[A] = buffer.toImage

  override def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

/** blend colors onto each other (use A channel for transparency) - respects depth: only fg pixels overlay bg pixels */
case class BlendingBufferWithDepth(override val width: Int, override val height: Int, background: RGBA, zInit: Double = Double.PositiveInfinity) extends RenderBuffer[RGBA] {
  private val buffer = ImageBuffer.makeConstantBuffer(width, height, background)
  private val zBuffer = ImageBuffer.makeConstantBuffer(width, height, zInit)

  override def update(x: Int, y: Int, z: Double, v: RGBA): Unit = {
    if (isDefinedAt(x, y)) {
      synchronized {
        if (z < zBuffer(x, y)) {
          buffer(x, y) = v over buffer(x, y)
          zBuffer(x, y) = z
        }
      }
    }
  }

  override def toImage: PixelImage[RGBA] = buffer.toImage

  override def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

/** blend colors onto each other (use A channel for transparency) */
case class BlendingBuffer(override val width: Int, override val height: Int, background: RGBA, zInit: Double = Double.PositiveInfinity) extends RenderBuffer[RGBA] {
  private val buffer = ImageBuffer.makeConstantBuffer(width, height, background)

  override def update(x: Int, y: Int, z: Double, v: RGBA): Unit = {
    if (isDefinedAt(x, y)) this.synchronized {
      buffer(x, y) = v over buffer(x, y)
    }
  }

  override def toImage: PixelImage[RGBA] = buffer.toImage

  override def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

/** simple plain render buffer, does not manage fragment collapse, last is kept */
case class PlainRenderBuffer[A: ClassTag](override val width: Int, override val height: Int, clearColor: A) extends RenderBuffer[A] {
  val buffer: ImageBuffer[A] = ImageBuffer.makeInitializedBuffer(width, height)(clearColor)

  override def update(x: Int, y: Int, z: Double, v: A): Unit = {
    if (isDefinedAt(x, y)) buffer(x, y) = v
  }

  override def toImage: PixelImage[A] = buffer.toImage

  override def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}