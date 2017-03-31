/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.faces.image.pyramid

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.AccessMode.MirroredPositionFunctional
import scalismo.faces.image.{InterpolationKernel, PixelImage}
import scalismo.faces.image.filter.{ImageFilter, ResampleFilter}

import scala.reflect.ClassTag

/**
  * Laplace pyramid implementation.
  *
  * @param imagePyramid used to construct the difference images.
  * @param expand A function that upscales the images.
  * @tparam A Pixel type of underlying images in the Pyramid.
  */
class LaplacePyramid[A: ClassTag](val imagePyramid: ImagePyramid[A], val expand: ImageFilter[A, A])(implicit ops: ColorSpaceOperations[A])
  extends ImagePyramid[A] {
  import PixelImage.implicits._

  override val levels: Int = imagePyramid.levels

  override val level: Seq[PixelImage[A]] = {
    val images = imagePyramid.level
    images.init.zip(images.tail).map(p => p._1 - expand(p._2)) :+ images.last
  }

  /**
    * Reconstructs the original image using the expand function and the addition of images based on the passed ColorSpaceOperations ops.
    */
  def reconstruct: PixelImage[A] = level.init.foldRight(level.last)((diff, combined) => expand(combined) + diff)
}

object LaplacePyramid {
  
  /**
    * Standard filter to be used to upscale the image.
    */
  def interpolationKernel = InterpolationKernel.BilinearKernel

  /**
    * Standard method to upscale an image.
    */
  def expand[A: ClassTag](implicit ops: ColorSpaceOperations[A]) = new ImageFilter[A, A] {
    override def filter(image: PixelImage[A]): PixelImage[A] = {
      val w = image.width * 2
      val h = image.height * 2

      import ColorSpaceOperations.implicits._
      ResampleFilter.resampleImage(image.withAccessMode(MirroredPositionFunctional((a:A, b:A)=>2*:a-b)), w, h, interpolationKernel)
    }
  }

  /**
    * Standard way to construct a LaplacePyramid.
    *
    * @param image image to build the laplace pyramid from
    * @param reductions number of desired levels (-1 gives the maximum allowed levels).
    */
  def apply[A: ClassTag](image: PixelImage[A], reductions: Int = -1)(implicit ops: ColorSpaceOperations[A]): LaplacePyramid[A] = {
    val imagePyramid = GaussPyramid(image, reductions)
    new LaplacePyramid[A](imagePyramid, expand)
  }
}
