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

/** manages access outside the image domain of a PixelImage */
trait AccessMode[A] {
  /** outside access */
  def apply(x: Int, y: Int, image: PixelImage[A]): A = outsideAccess(x, y, image)

  /** outside access */
  def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A

  /** transform type */
  def map[B](f: A => B): AccessMode[B]
}

/// Provides various typical image access boundary conditions
object AccessMode {
  case class Padded[A](outsideValue: A) extends AccessMode[A] {
    override def map[B](f: (A) => B): AccessMode[B] = Padded(f(outsideValue))

    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = {
      if (image.domain.isDefinedAt(x, y))
        image(x, y)
      else
        outsideValue
    }
  }

  case class Mirror[A]() extends AccessMode[A] {
    override def map[B](f: (A) => B): AccessMode[B] = Mirror()

    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = {
      val width = image.width
      val height = image.height
      // reduce coordinates to 2*w, 2*h tile (basis of mirror image crystal)
      val px = posMod(x, 2 * width)
      val py = posMod(y, 2 * height)
      // calculate mirrored coordinates
      image(mirror(px, width), mirror(py, height))
    }

    @inline
    def posMod(i: Int, m: Int): Int = if (i >= 0) i % m else (i % m + m) % m

    @inline
    def mirror(i: Int, m: Int): Int = if (i < m) i else 2 * m - i - 1
  }

  /// accesses image in a periodic crystal grid
  case class Periodic[A]() extends AccessMode[A] {
    override def map[B](f: (A) => B): AccessMode[B] = Periodic()

    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = {
      val width = image.width
      val height = image.height
      image(posMod(x, width), posMod(y, height))
    }

    @inline
    def posMod(i: Int, m: Int): Int = if (i >= 0) i % m else (i % m + m) % m
  }

  /// repeats borders outside the image
  case class Repeat[A]() extends AccessMode[A] {
    override def map[B](f: (A) => B): AccessMode[B] = Repeat()

    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = {
      val width = image.width
      val height = image.height
      def clamp(i: Int, min: Int, max: Int): Int = math.min(max, math.max(min, i))
      image(clamp(x, 0, width - 1), clamp(y, 0, height - 1))
    }
  }

  /// restrict access to inner part of domain
  case class Strict[A]() extends AccessMode[A] {
    override def map[B](f: (A) => B): AccessMode[B] = Strict()

    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = {
      if (image.domain.isDefinedAt(x, y))
        image(x, y)
      else
        throw new Exception(s"image access outside domain: ($x, $y), size=(${image.width}, ${image.height})")
    }
  }

  /**
    * This is used by the gauss & laplace pyramid as border condition.
    * Usually this is used with the combine function: (a,b) => 2*a-b.
    *
    * @param combine Functional to combine the two values at positions.
    */
  case class MirroredPositionFunctional[A](combine: (A,A) => A) extends AccessMode[A] {
    override def map[B](f: (A) => B): AccessMode[B] = ???

    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = {
      if (image.domain.isDefinedAt(x, y))
        image(x, y)
      else {
        val dx = if (x < 0) Some((0, clamp(x * (-1),0,image.width-1))) else if (x >= image.width) Some((image.width - 1, clamp(2 * (image.width - 1) - x,0,image.width-1))) else None
        val dy = if (y < 0) Some((0, clamp(y * (-1),0,image.height-1))) else if (y >= image.height) Some((image.height - 1, clamp(2 * (image.height - 1) - y,0,image.height-1))) else None
        if (dx.isDefined && dy.isDefined) {
          val px = dx.get
          val py = dy.get
          combine(image.valueAt(px._1, py._1), image.valueAt(px._2, py._2))
        } else if (dx.isDefined) {
          val px = dx.get
          combine(image.valueAt(px._1, y), image.valueAt(px._2, y))
        } else if (dy.isDefined) {
          val py = dy.get
          combine(image.valueAt(x, py._1),image.valueAt(x, py._2))
        } else {
          throw new Exception(s"image access mode was outside but now inside.... confusing: ($x, $y), size=(${image.width}, ${image.height})")
        }
      }
    }

    private def clamp(i: Int, min: Int, max: Int): Int = math.min(max, math.max(min, i))
  }

  /** delegate outside access to given function */
  case class Functional[A](f: (Int, Int) => A) extends AccessMode[A] {
    override def outsideAccess(x: Int, y: Int, image: PixelImage[A]): A = f(x, y)

    override def map[B](f: (A) => B): AccessMode[B] = Functional((x, y) => f(this.f(x, y)))
  }
}
