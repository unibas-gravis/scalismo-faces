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

import scalismo.color.ColorSpaceOperations
import scalismo.color.ColorSpaceOperations.implicits._

import scala.reflect.ClassTag

/** basic differential operations on PixelImage[A], not specialized, need ColorSpaceOperations[A] */
object PixelImageDifferential {

  /** gradient, x direction, central finite difference */
  def gradX[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val infImage = image.withAccessMode(AccessMode.Repeat())
    PixelImage(image.domain, (x, y) => 0.5f *: (infImage(x + 1, y) - infImage(x - 1, y)))
  }

  /** gradient, x direction, positive direction finite difference */
  def gradXp[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val infImage = image.withAccessMode(AccessMode.Repeat())
    PixelImage(image.domain, (x, y) => infImage(x + 1, y) - infImage(x, y))
  }

  /** gradient, x direction, negative direction finite difference */
  def gradXm[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val infImage = image.withAccessMode(AccessMode.Repeat())
    PixelImage(image.domain, (x, y) => infImage(x, y) - infImage(x - 1, y))

  }

  /** gradient, y direction, central finite difference */
  def gradY[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val infImage = image.withAccessMode(AccessMode.Repeat())
    PixelImage(image.domain, (x, y) => 0.5f *: (infImage(x, y + 1) - infImage(x, y - 1)))

  }

  /** gradient, y direction, positive direction finite difference */
  def gradYp[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val infImage = image.withAccessMode(AccessMode.Repeat())
    PixelImage(image.domain, (x, y) => infImage(x, y + 1) - infImage(x, y))
  }

  /** gradient, y direction, negative direction finite difference */
  def gradYm[A: ClassTag](image: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val infImage = image.withAccessMode(AccessMode.Repeat())
    PixelImage(image.domain, (x, y) => infImage(x, y) - infImage(x, y - 1))
  }

  /** divergence div(f), expects separate images for x and y components of f */
  def div[A: ClassTag](x: PixelImage[A], y: PixelImage[A])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    require(x.domain == y.domain)
    val dXdx = gradX(x)
    val dYdy = gradY(y)
    PixelImage(x.domain, (x, y) => dXdx(x, y) + dYdy(x, y))
  }

  /** divergence div(f), (A, A) contains (x, y) components of vector field f */
  def div[A: ClassTag](f: PixelImage[(A, A)])(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    val dXdx = gradX(f.map(_._1))
    val dYdy = gradY(f.map(_._2))
    PixelImage(f.domain, (x, y) => dXdx(x, y) + dYdy(x, y))
  }

  def laplace4NN[A: ClassTag](image: PixelImage[A], dx: Double = 1.0)(implicit ops: ColorSpaceOperations[A]): PixelImage[A] = {
    PixelImage(image.domain, (x, y) =>
      (image(x - 1, y) + image(x + 1, y) + image(x, y - 1) + image(x, y + 1) - 4 *: image(x, y)) / (dx * dx)
    )
  }
}
