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

import scalismo.color.ColorSpaceOperations
import scalismo.faces.image.filter.{ImageFilter, IsotropicGaussianFilter, ResampleFilter}
import scalismo.faces.image.{AccessMode, InterpolationKernel, PixelImage}

import scala.reflect._

/**
  * GaussPyramid implements the concept of images always reduced to half the size of the image.
  *
  * @param image image to build the pyramid with.
  * @param reduce the operation that reduces the image one level
  * @param reductions the number of reductions that should be built. A negative number means as many as possible.
  * @tparam A Pixel type of underlying images in the Pyramid.
  */
class GaussPyramid[A: ClassTag](val image: PixelImage[A], val reduce: ImageFilter[A, A], val reductions: Int)(implicit ops: ColorSpaceOperations[A])
  extends ImagePyramid[A] {

  override val level: Seq[PixelImage[A]] = {
    def makeReducedImages(image: PixelImage[A], levels: Int): Seq[PixelImage[A]] = {
      if (levels == 0) Seq(image)
      else {
        val reduced = reduce.filter(image)
        if(reduced.width > 0 && reduced.height > 0)
          image +: makeReducedImages(reduced, levels - 1)
        else Seq()
      }
    }

    makeReducedImages(image, reductions)
  }

  override val levels = level.size
}

object GaussPyramid {

  /**
    * Standard filter operation used before subsampling.
    */
  def filter[A: ClassTag](implicit ops: ColorSpaceOperations[A]): IsotropicGaussianFilter[A] = IsotropicGaussianFilter[A](2)

  /**
    * Standard reduce operation dividing the image size along each dimension by 2.
    */
  def reduce[A: ClassTag](implicit ops: ColorSpaceOperations[A]) = reduceScaled[A](0.5)

  /**
    * Returns an image filter that reduces an image according to a scaling factor. The scaling factor is computed as follows:
    * scaleNumerator / scaleDenominator
    * @param scale The reduction applied to each level. The number should lie in the range (0,1) and is approximated to the closest rational number corresponding to the size of neighboring levels.
    */
  def reduceScaled[A: ClassTag](scale: Double)(implicit ops: ColorSpaceOperations[A]) = new ImageFilter[A, A] {
    require( scale>0.0 && scale <1.0, "scale must be on (0,1.0). scale= scaleNumerator/scaleDenominator" )

    import ColorSpaceOperations.implicits._

    def interpolationKernel = InterpolationKernel.BilinearKernel

    override def filter(img: PixelImage[A]): PixelImage[A] = {
      val w = (img.width * scale).toInt
      val h = (img.height * scale).toInt
      val filteredImage = img.withAccessMode(AccessMode.MirroredPositionFunctional((a: A, b: A) => 2 *: a - b)).filter(GaussPyramid.filter)
      if ( w > 0 && h > 0) {
        ResampleFilter.resampleImage(filteredImage,w,h,interpolationKernel)
      } else {
        PixelImage[A](0, h, (x: Int, y: Int) => throw new RuntimeException)
      }
    }
  }

  /**
    * Standard way to create an image pyramid.
    * @param image base images
    * @param reductions desired reductions (-1 for as many as possible), which night not be the actual reductions.
    */
  def apply[A: ClassTag](image: PixelImage[A], reductions: Int = -1)(implicit ops: ColorSpaceOperations[A]): GaussPyramid[A] = {
    new GaussPyramid[A](image, reduce, reductions)
  }
}
