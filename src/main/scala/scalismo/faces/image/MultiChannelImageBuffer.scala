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

import scalismo.common.ComponentRepresentation

import scala.reflect.ClassTag

/** fast raw image with multiple Double channels */
class MultiChannelImageBuffer private (val width: Int, val height: Int, val channels: Int, private val data: Array[Double]) {

  val length: Int = width * height * channels

  // channel / column major access
  private def index(x: Int, y: Int, c: Int): Int = x * height * channels + y * channels + c

  /** access value at (x, y), in channel c */
  def apply(x: Int, y: Int, c: Int): Double = data(index(x, y, c))

  /** update value at (x, y), in channel c */
  def update(x: Int, y: Int, c: Int, v: Double): Unit = data(index(x, y, c)) = v

  /** get all channels as Array at (x, y) (read-only) */
  def channelSlice(x: Int, y: Int): Array[Double] = data.slice(index(x, y, 0), index(x, y, channels))

  /** get all channels as Array at (x, y) (read-only) */
  def updateChannelSlice(x: Int, y: Int, slice: Array[Double]): Unit = {
    require(slice.length == channels)
    var c = 0
    while (c < channels) {
      data(index(x, y, c)) = slice(c)
      c += 1
    }
  }

  /** convert this MultiChannelImageBuffer to a standard PixelImage, needs a Vectorizer[A] */
  def toImage[A: ClassTag](implicit vec: ComponentRepresentation[A]): PixelImage[A] = toUnsafeImage.buffer

  /** generate a view of this buffer, WARNING: updates of buffer are propagated */
  def toUnsafeImage[A: ClassTag](implicit vec: ComponentRepresentation[A]): PixelImage[A] = {
    require(vec.size == channels, "vectorization with wrong number of channels")
    PixelImage.view(width, height, (x, y) => vec.fromComponents(i => this(x, y, i)))
  }

  /** clone this buffer */
  override def clone = new MultiChannelImageBuffer(width, height, channels, data.clone())

  /** content-aware equality */
  override def equals(other: Any): Boolean = other match {
    case mcb: MultiChannelImageBuffer => width == mcb.width && height == mcb.height && channels == mcb.channels && data.deep == mcb.data.deep
    case _ => false
  }
}

object MultiChannelImageBuffer {
  /** generate empty buffer of given size */
  def apply(width: Int, height: Int, channels: Int): MultiChannelImageBuffer = {
    MultiChannelImageBuffer(width, height, channels, new Array[Double](width * height * channels))
  }

  /** generate from existing data array (channels / column major) */
  def apply(width: Int, height: Int, channels: Int, data: Array[Double]): MultiChannelImageBuffer = {
    require(data.length == width * height * channels, "data array has wrong length")
    new MultiChannelImageBuffer(width, height, channels, data)
  }

  /** fill buffer with contents of image, needs a Vectorizer[A] to transform A to Array[Double] */
  def vectorize[A](image: PixelImage[A])(implicit vec: ComponentRepresentation[A]): MultiChannelImageBuffer = {
    val d = vec.size
    val data = new Array[Double](image.width * image.height * d)
    val buf = new MultiChannelImageBuffer(image.width, image.height, d, data)
    // fill data
    var y = 0
    while (y < buf.height) {
      var x = 0
      while (x < buf.width) {
        val value = image(x, y)
        var c = 0
        while (c < d) {
          buf(x, y, c) = vec.component(value, c)
          c += 1
        }
        x += 1
      }
      y += 1
    }
    buf
  }
}