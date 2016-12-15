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

package scalismo.faces.image.filter

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.{PixelImage, PixelImageDomain}

import scala.reflect.ClassTag

object Gradient {
  def sobelX[Pixel: ClassTag](implicit ops: ColorSpaceOperations[Pixel]): SeparableCorrelationFilter[Pixel] = {
    val row: PixelImage[Double] = PixelImage(PixelImageDomain(3, 1), Array(-1.0, 0.0, 1.0))
    val col: PixelImage[Double] = PixelImage(PixelImageDomain(1, 3), Array(1.0, 2.0, 1.0))
    SeparableCorrelationFilter[Pixel](row, col)
  }
  def sobelY[Pixel: ClassTag](implicit ops: ColorSpaceOperations[Pixel]): SeparableCorrelationFilter[Pixel] = {
    val row: PixelImage[Double] = PixelImage(PixelImageDomain(3, 1), Array(1.0, 2.0, 1.0))
    val col: PixelImage[Double] = PixelImage(PixelImageDomain(1, 3), Array(-1.0, 0.0, 1.0))
    SeparableCorrelationFilter[Pixel](row, col)
  }
  def scharrX[Pixel: ClassTag](implicit ops: ColorSpaceOperations[Pixel]): SeparableCorrelationFilter[Pixel] = {
    val row: PixelImage[Double] = PixelImage(PixelImageDomain(3, 1), Array(-1.0, 0.0, 1.0))
    val col: PixelImage[Double] = PixelImage(PixelImageDomain(1, 3), Array(3.0, 10.0, 3.0))
    SeparableCorrelationFilter[Pixel](row, col)
  }
  def scharrY[Pixel: ClassTag](implicit ops: ColorSpaceOperations[Pixel]): SeparableCorrelationFilter[Pixel] = {
    val row: PixelImage[Double] = PixelImage(PixelImageDomain(3, 1), Array(3.0, 10.0, 3.0))
    val col: PixelImage[Double] = PixelImage(PixelImageDomain(1, 3), Array(-1.0, 0.0, 1.0))
    SeparableCorrelationFilter[Pixel](row, col)
  }
}
