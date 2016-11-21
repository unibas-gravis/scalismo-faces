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

/** abstraction  of a PixelImage index structure: mapping between linear and 2d access */
sealed trait PixelImageDomain {
  lazy val points: IndexedSeq[(Int, Int)] = for (i <- 0 until length) yield (x(i), y(i))
  lazy val indices: Range = 0 until length
  val width: Int
  val height: Int
  val size: (Int, Int) = (width, height)
  val length: Int = width * height

  def transpose: PixelImageDomain

  def other: PixelImageDomain

  def toColumnMajor: PixelImageDomain

  def toRowMajor: PixelImageDomain

  def coordsFromIndex(index: Int): (Int, Int) = (x(index), y(index))

  def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

  def index(x: Int, y: Int): Int

  def x(index: Int): Int

  def y(index: Int): Int
}

object PixelImageDomain {
  def apply(width: Int, height: Int) = ColumnMajorImageDomain(width, height)
}

/** abstraction  of a PixelImage index structure: mapping between linear and 2d access, column major */
case class ColumnMajorImageDomain(override val width: Int, override val height: Int) extends PixelImageDomain {
  override def index(x: Int, y: Int): Int = x * height + y

  override def x(index: Int): Int = index / height

  override def y(index: Int): Int = index % height

  override def transpose: ColumnMajorImageDomain = ColumnMajorImageDomain(height, width)

  override def other = RowMajorImageDomain(width, height)

  override def toColumnMajor: PixelImageDomain = this

  override def toRowMajor: PixelImageDomain = other
}

/** abstraction  of a PixelImage index structure: mapping between linear and 2d access, row major */
case class RowMajorImageDomain(override val width: Int, override val height: Int) extends PixelImageDomain {
  override def index(x: Int, y: Int): Int = x + y * width

  override def x(index: Int): Int = index % width

  override def y(index: Int): Int = index / width

  override def transpose: RowMajorImageDomain = RowMajorImageDomain(height, width)

  override def toColumnMajor: PixelImageDomain = other

  override def toRowMajor: PixelImageDomain = this

  override def other = ColumnMajorImageDomain(width, height)
}