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

package scalismo.faces.image

import scalismo.faces.color.ColorSpaceOperations
import scalismo.faces.image.AccessMode.Repeat

import scala.reflect.ClassTag

/** Push-Pull interpolation */
object PushPullInterpolation {

  /**
    * Performs push-pull interpolation: reconstruct missing values (mask==0) with average values from upper pyramid levels
    * mask indicates the validity of values, 1.0 fully valid, 0.0 invalid - values in [0, 1] are ok
    * @param image image to interpolate / fill
    * @param mask mask indicating validity of color values in image, mask==1: value is valid - will not be altered, mask==0: invalid - will be replaced by averages of valid values
    * @param minSize minimal image size of pyramid (defines levels of pyramid)
    */
  def fill[A: ClassTag](image: PixelImage[A], mask: PixelImage[Double], minSize: Int = 1)(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    if (image.width > minSize && image.height > minSize) {
      // fill next lower level and expand to this size
      val lowerLevel = fill(shrink2WithMask(image, mask), shrink2(mask), minSize)
      val lowerLevelGrown = lowerLevel.interpolate.sample(image.width, image.height)
      // mix between lowerLevel and this one according to mask
      PixelImage(image.domain, (x, y) => ops.blend(image(x, y), lowerLevelGrown(x, y), mask(x, y))).withAccessMode(image.accessMode)
    } else { // smallest unit reached
      image
    }
  }

  private def shrink2WithMask[A: ClassTag](image: PixelImage[A], mask: PixelImage[Double])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    import ColorSpaceOperations.implicits._
    val (w, h) = (image.width/2, image.height/2)
    val imageWithMask = image.zip(mask)
    val maskedImage = imageWithMask.map{case (color, weight) => color * weight}
    val shrink2 = maskedImage.resample(w, h).withAccessMode(Repeat())
    val shrink2Mask = mask.resample(w, h).withAccessMode(Repeat())
    // scale where not every pixel counted
    shrink2.zip(shrink2Mask)
      .map{ case (weightedColor, avgWeight) =>
        if (avgWeight > 1e-8)
          weightedColor / avgWeight
        else
          weightedColor}
      .withAccessMode(Repeat())
  }

  private def shrink2[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    image.resample(image.width/2, image.height/2).withAccessMode(Repeat())
  }
}
