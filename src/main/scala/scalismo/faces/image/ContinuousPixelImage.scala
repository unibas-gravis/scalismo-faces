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

import scala.reflect.ClassTag

/** continuous access image */
trait ContinuousPixelImage[Pixel] extends ((Double, Double) => Pixel) {
  val width: Double
  val height: Double

  /**
   * Image value at specified position (x, y) - this is not the pixel position but the coordinates of a point! The
   * center of pixel (0, 0) lies at (0.5, 0.5) in the coordinate system (cell-centered)
   * @param x
   *   x coordinate (0.5 <-> 0 in discrete case)
   * @param y
   *   y coordinate (0.5 <-> 0 in discrete case)
   */
  override def apply(x: Double, y: Double): Pixel

  /**
   * Access the image value at the "continuous" pixel position, (0.0, 0.0) corresponds to pixel at (0, 0)
   * @param x
   *   continuous pixel coordinate (0.0 <-> 0 in discrete case)
   * @param y
   *   continuous pixel coordinate (0.0 <-> 0 in discrete case)
   */
  def atContinuousPixel(x: Double, y: Double): Pixel = this(x + 0.5, y + 0.5)

  /**
   * sample the continuous image to produce a discrete PixelImage
   *
   * @param w
   *   Number of samples along x axis (width of image)
   * @param h
   *   Number of samples along y axis (height of image)
   */
  def sample(w: Int, h: Int)(implicit tag: ClassTag[Pixel]): PixelImage[Pixel] = {
    require(w > 0.0 && h > 0.0)
    // scale factor
    val scaleW = width / w
    val scaleH = height / h
    PixelImage(w,
               h,
               (i, j) => {
                 // center point for access
                 val x = i + 0.5
                 val y = j + 0.5
                 this(x * scaleW, y * scaleH)
               }
    )
  }

  /** apply function to each pixel */
  def map[B](g: Pixel => B): ContinuousPixelImage[B]

  /** andThen is identical to map, compatibility with scalismo interface */
  def andThen[B](g: Pixel => B): ContinuousPixelImage[B] = map(g)
}

/** function image with continuous access */
case class Function2DImage[A](override val width: Double, override val height: Double, f: (Double, Double) => A)
    extends ContinuousPixelImage[A] {

  override def apply(x: Double, y: Double): A = f(x, y)

  /** apply function to each pixel */
  override def map[B](g: (A) => B): ContinuousPixelImage[B] = Function2DImage(width, height, (x, y) => g(f(x, y)))
}
