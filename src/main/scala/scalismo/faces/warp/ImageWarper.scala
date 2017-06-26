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

package scalismo.faces.warp

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.AccessMode.Strict
import scalismo.faces.image.PixelImage
import scalismo.geometry.{Vector, _2D}

import scala.reflect.ClassTag

/** warp images with a warp field */
object ImageWarper {
  type WarpField = PixelImage[Vector[_2D]]

  /** set boundary of warp field to Vector(0, 0) - warp leaves boundary unchanged ("stops" at image border) */
  def constantWarpBoundary(image: PixelImage[Option[Vector[_2D]]]): PixelImage[Option[Vector[_2D]]] = {
    def isBorder(x: Int, y: Int) = x == 0 || y == 0 || x == image.width - 1 || y == image.height - 1
    val borderWarp: Option[Vector[_2D]] = Some(Vector(0f, 0f))
    PixelImage.fromTemplate(image, (x, y) => if (isBorder(x, y)) borderWarp else image(x, y))
  }

  /** standard image warp (backwarp), warp field defines for each new pixel where it comes from */
  def warpImage[A: ClassTag](original: PixelImage[A], warpField: PixelImage[Vector[_2D]])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val contImage = original.interpolate
    PixelImage(warpField.width, warpField.height, (x, y) => {
      val w = warpField(x, y)
      contImage(x + w.x + 0.5, y + w.y + 0.5)
    }).buffer.withAccessMode(Strict())
  }

  /** forward mapping of each pixel, inverts the field and uses backwarp */
  def warpImageForward[A: ClassTag](original: PixelImage[A], warpField: PixelImage[Vector[_2D]])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    require(original.domain == warpField.domain, "forward warp needs identical image and warp field domains")
    val invertedWarp = WarpFieldInversion.fixedPointInversion(warpField)
    warpImage(original, invertedWarp)
  }
}
