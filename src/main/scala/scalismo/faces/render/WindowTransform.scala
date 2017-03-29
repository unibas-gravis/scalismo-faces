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

package scalismo.faces.render

import scalismo.geometry.{Point, Vector, _3D}

/** transform a point from normalized coordinates [-1,1]x[-1,1]x[-1,1] to window coordinates [0,w]x[0,h]x[near,far] (y still upwards!) used by renderer */
case class WindowTransform(width: Int, height: Int, near: Double = 0.0, far: Double = 1.0) extends InvertibleTransform3D {
  override def apply(p: Point[_3D]): Point[_3D] = Point(
    width / 2.0 * (p.x + 1.0),
    height / 2.0 * (-p.y + 1.0),
    (far - near) / 2.0 * p.z + (far + near) / 2.0
  )

  /** apply transform to a 3d vector */
  override def apply(v: Vector[_3D]): Vector[_3D] = Vector(
    width / 2.0 * (v.x + 1.0),
    height / 2.0 * (-v.y + 1.0),
    (far - near) / 2.0 * v.z + (far + near) / 2.0
  )

  /** inverted version of this transform */
  override def inverted: InvertibleTransform3D = InverseWindowTransform(width, height, near, far)
}

/** inverse window transform: from [0,width]x[0,height]x[near,far] to [-1,1]x[-1,1]x[-1,1] */
case class InverseWindowTransform(width: Int, height: Int, near: Double = 0.0, far: Double = 1.0) extends InvertibleTransform3D {
  override def apply(x: Point[_3D]): Point[_3D] = Point(
    x.x * 2.0 / width - 1,
    - (x.y * 2.0 / height - 1),
    (x.z - (far + near) / 2) * 2.0 / (far - near)
  )

  /** apply transform to a 3d vector */
  override def apply(v: Vector[_3D]): Vector[_3D] = Vector(
    v.x * 2.0 / width - 1,
    - (v.y * 2.0 / height - 1),
    (v.z - (far + near) / 2) * 2.0 / (far - near)
  )

  override def inverted: InvertibleTransform3D = WindowTransform(width, height, near, far)
}


/** Use for rendering only the contents within a defined bounding box.
  * transforms a point from normalized coordinates [-1,1]x[-1,1]x[-1,1] to window coordinates [0,widthBox]x[0,heightBox]x[near,far] (y still upwards!)
  *
  * @param width of the full image
  * @param height of the full image
  * @param widthBox
  * @param heightBox
  * @param posX top right corner of box
  * @param posY top right corner of box
  * @param near
  * @param far
  */
case class WindowBoxTransform(width: Int, height: Int, widthBox: Int, heightBox: Int, posX: Int, posY: Int, near: Double = 0.0, far: Double = 1.0) extends InvertibleTransform3D {

  /**
    * ratioWidth/Height: scales full image width to box width
    */
  val ratioWidth: Double = width.toDouble / widthBox.toDouble / 2.0
  val ratioHeight: Double = height.toDouble / heightBox.toDouble / 2.0

  /**
    * box scaled normalized coordinates go from [-1*ratioWidth/2 to 1*ratioWidth/2]
    * boxScaledCoords = ((posX.toDouble / width.toDouble)-0.5) * ratioWidth
    * the box is located at (xInBoxScaledCoords, yInBoxScaleCoords)
    */
  val xInBoxScaledCoords: Double = (1.0 / widthBox.toDouble) * (posX.toDouble - width / 2.0)
  val yInBoxScaledCoords: Double = (1.0 / heightBox.toDouble) * (posY.toDouble - height / 2.0)

  override def apply(p: Point[_3D]): Point[_3D] = Point(
    (p.x * ratioWidth - xInBoxScaledCoords) * widthBox,
    (-p.y * ratioHeight - yInBoxScaledCoords) * heightBox,
    (far - near) / 2.0 * p.z + (far + near) / 2.0 // z same as WindowTransform
  )

  override def apply(p: Vector[_3D]): Vector[_3D] = Vector(
    (p.x * ratioWidth - xInBoxScaledCoords) * widthBox,
    (-p.y * ratioHeight - yInBoxScaledCoords) * heightBox,
    (far - near) / 2.0 * p.z + (far + near) / 2.0 // z same as WindowTransform
  )

  override def inverted: InverseWindowBoxTransform = InverseWindowBoxTransform(width, height, widthBox, heightBox, posX, posY, near, far)
}

case class InverseWindowBoxTransform(width: Int, height: Int, widthBox: Int, heightBox: Int, posX: Int, posY: Int, near: Double = 0.0, far: Double = 1.0) extends InvertibleTransform3D {
  val ratioWidth: Double = width.toDouble / widthBox.toDouble / 2.0
  val ratioHeight: Double = height.toDouble / heightBox.toDouble / 2.0
  val xInBoxScaledCoords: Double = (1.0 / widthBox.toDouble) * (posX.toDouble - width / 2.0)
  val yInBoxScaledCoords: Double = (1.0 / heightBox.toDouble) * (posY.toDouble - height / 2.0)

  override def apply(p: Point[_3D]): Point[_3D] = Point(
    (p.x / widthBox + xInBoxScaledCoords) / ratioWidth,
    -(p.y / heightBox + yInBoxScaledCoords) / ratioHeight,
    (p.z - (far + near) / 2) * 2.0 / (far - near) // z same as WindowTransform
  )

  override def apply(p: Vector[_3D]): Vector[_3D] = Vector(
    (p.x / widthBox + xInBoxScaledCoords) / ratioWidth,
    -(p.y / heightBox + yInBoxScaledCoords) / ratioHeight,
    (p.z - (far + near) / 2) * 2.0 / (far - near) // z same as WindowTransform
  )

  override def inverted: WindowBoxTransform = WindowBoxTransform(width, height, widthBox, heightBox, posX, posY, near, far)
}
