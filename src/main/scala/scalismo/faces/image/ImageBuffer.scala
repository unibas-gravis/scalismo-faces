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

import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag

/** Image buffer for mutable creation of an image, use toImage when finished to convert to a standard immutable image */
class ImageBuffer[@specialized(Int, Float, Double) Pixel: ClassTag](val domain: PixelImageDomain,
                                                                    private var data: Array[Pixel]
) {
  def width: Int = domain.width
  def height: Int = domain.height

  require(data.length == domain.length)

  private var safeToWrite: Boolean = true

  /// execute f only if it is safe to do so, if not, clone data first
  private def copyOnWrite(f: => Unit) = {
    if (safeToWrite)
      f
    else {
      data = data.clone()
      safeToWrite = true
      f
    }
  }

  /// read value at Index
  def apply(x: Int, y: Int): Pixel = data(domain.index(x, y))

  private def rawUpdate(x: Int, y: Int, value: Pixel): Unit = {
    data(domain.index(x, y)) = value
  }

  /// Set pixel value for index
  def update(x: Int, y: Int, value: Pixel): Unit = copyOnWrite(rawUpdate(x, y, value))

  private def rawTransform(f: Pixel => Pixel): Unit = (0 until domain.length).foreach(i => data(i) = f(data(i)))

  /// in-place transformation
  def transform(f: Pixel => Pixel): Unit = copyOnWrite(rawTransform(f))

  private def rawTransformWithIndex(f: (Int, Int, Pixel) => Pixel): Unit =
    (0 until domain.length).foreach(i => data(i) = f(domain.x(i), domain.y(i), data(i)))

  /// in-place transformation
  def transformWithIndex(f: (Int, Int, Pixel) => Pixel): Unit = copyOnWrite(rawTransformWithIndex(f))

  private def rawTransformWithIndexParallel(f: (Int, Int, Pixel) => Pixel): Unit =
    ParVector.range(0, domain.length).foreach(i => data(i) = f(domain.x(i), domain.y(i), data(i)))

  /// in-place transformation, parallel execution
  def transformWithIndexParallel(f: (Int, Int, Pixel) => Pixel): Unit = copyOnWrite(rawTransformWithIndexParallel(f))

  def copyData: Array[Pixel] = data.clone()

  private def rawFill(f: => Pixel): Unit = (0 until domain.length).foreach(i => data(i) = f)

  /// fill buffer with value
  def fill(f: => Pixel): Unit = copyOnWrite(rawFill(f))

  /** make a read-only copy, copy is cheap, implemented as copy-on-write on next mutable operation */
  def toImage: PixelImage[Pixel] = {
    safeToWrite = false // mark data as unsafe to be mutated
    PixelImage(domain, data)
  }

  /**
   * *unsafe* make a read-only copy which is an unsafe view of this buffer, changes with every mutable operation!
   */
  def toUnsafeImage: PixelImage[Pixel] = {
    PixelImage(domain, data)
  }
}

object ImageBuffer {

  def apply[@specialized(Int, Float, Double) Pixel: ClassTag](width: Int,
                                                              height: Int,
                                                              data: Array[Pixel]
  ): ImageBuffer[Pixel] = {
    new ImageBuffer[Pixel](PixelImageDomain(width, height), data)
  }

  def apply[@specialized(Int, Float, Double) Pixel: ClassTag](domain: PixelImageDomain,
                                                              data: Array[Pixel]
  ): ImageBuffer[Pixel] = {
    new ImageBuffer[Pixel](domain, data)
  }

  def apply[Pixel: ClassTag](image: PixelImage[Pixel]): ImageBuffer[Pixel] = image.toBuffer

  /// Create an uninitialized image buffer, 2D image
  def makeEmptyBuffer[Pixel: ClassTag](width: Int, height: Int): ImageBuffer[Pixel] = {
    // create storage: Array
    val data = new Array[Pixel](width * height)
    ImageBuffer(width, height, data)
  }

  /// Create initialized image buffer, 2D image
  def makeInitializedBuffer[Pixel: ClassTag](width: Int, height: Int)(f: => Pixel): ImageBuffer[Pixel] = {
    // create storage: Array
    val data = Array.fill(width * height)(f)
    ImageBuffer(width, height, data)
  }

  /// Create initialized image buffer, 2D image
  def makeConstantBuffer[Pixel: ClassTag](width: Int, height: Int, value: Pixel): ImageBuffer[Pixel] = {
    // create storage: Array
    val data = new Array[Pixel](width * height)
    var i = 0
    while (i < data.length) {
      data(i) = value
      i += 1
    }
    ImageBuffer(width, height, data)
  }

  /// initialize buffer by tabulating a function
  def tabulate[Pixel: ClassTag](width: Int, height: Int)(f: (Int, Int) => Pixel): ImageBuffer[Pixel] = {
    // create storage: uninitialized Array
    val data = new Array[Pixel](width * height)
    val buffer = ImageBuffer(width, height, data)
    buffer.transformWithIndexParallel((x: Int, y: Int, _: Pixel) => f(x, y))
    buffer
  }

}
