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

import scalismo.geometry.Point._
import scalismo.geometry.{Point, EuclideanVector, _2D}

/** viewing frustum for camera projections, defines the viewing volume, see also: OpenGL */
case class Frustum(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double) {
  require(left < right, "left is right of right")
  require(bottom < top, "bottom is above top")
  require(near < far, "near is further than far")

  /**
   * get closest orthographic frustum, matches the behaviour at the given z distance, defaults to near plane
   *
   * @param z z position of plane with identical transformation behavior (depth, units identical to near and far)
   */
  def closestOrthographic(z: Double = near): Frustum = {
    Frustum(left * z / near, right * z / near, bottom * z / near, top * z / near, near, far)
  }

  /**
   * move scene in this frustum through an off-center projection, center point in NDC [-1,1]x[-1,1]
   *
   * @param center center position, in NDC [-1, 1]x[-1, 1] (outside is possible)
   */
  def withCenter(center: Point[_2D]): Frustum = {
    val Frustum(l, r, b, t, n, f) = centered
    Frustum(l * (center.x + 1), r * (1 - center.x), b * (center.y + 1), t * (1 - center.y), n, f)
  }

  /** center of projection expressed in NDC of the corresponding symmetric frustum */
  def center = Point((2 * -left / (right - left)) - 1, (2 * -bottom / (top - bottom)) - 1)

  /** center this frustum (symmetrize), balances left/right and top/bottom */
  def centered: Frustum = {
    val w = right - left
    val h = top - bottom
    Frustum(-w / 2, w / 2, -h / 2, h / 2, near, far)
  }

  /**
   * scale this frustum (near plane is not scaled), larger frustum -> smaller image
   *
   * @param horizontal horizontal scale factor
   * @param vertical   vertical scale factor
   */
  def scale(horizontal: Double, vertical: Double): Frustum = Frustum(left * horizontal, right * horizontal, bottom * vertical, top * vertical, near, far)

  /**
   * scale this frustum (near plane is not scaled), larger frustum -> smaller image
   *
   * @param f scale factor
   */
  def scale(f: Double): Frustum = scale(f, f)

  /** change the near clipping plane, leave the image view scaling unaltered */
  def withNear(near: Double): Frustum = {
    require(near < far, "near is farther than far")
    val f = near / this.near
    Frustum(left * f, right * f, bottom * f, top * f, near, far)
  }
}

object Frustum {
  import scalismo.geometry.EuclideanVector._
  /**
   * construct frustum from field of view, both horizontal and vertical
   *
   * @param fovX horizontal field of view (radians)
   * @param fovY vertical field of view (radians)
   * @param near near plane (absolute value)
   * @param far  far plane (absolute value)
   */
  def fromFOV(fovX: Double, fovY: Double, near: Double, far: Double): Frustum = {
    require(fovX > 0.0, "field of view must be positive")
    require(fovY > 0.0, "field of view must be positive")
    val width = math.abs(near) * math.tan(fovX / 2)
    val height = math.abs(near) * math.tan(fovY / 2)
    Frustum(-width / 2, width / 2, -height / 2, height / 2, near, far)
  }

  /**
   * construct frustum from vertical field of view and aspect ratio (w/h)
   *
   * @param fovY   vertical field of view (radians)
   * @param aspect aspect ratio: width/height
   * @param near   near plane (absolute value)
   * @param far    far plane (absolute value)
   */
  def fromVerticalFOV(fovY: Double, aspect: Double, near: Double, far: Double): Frustum = {
    fromFocal(1.0 / math.tan(fovY / 2), aspect, near, far)
  }

  /**
   * construct frustum from focal length and aspect ratio (w/h)
   *
   * @param focalLength focal length corresponding to a unit sensor size of 1
   * @param aspect      aspect ratio: width/height
   * @param near        near plane (absolute value)
   * @param far         far plane (absolute value)
   */
  def fromFocal(focalLength: Double, aspect: Double, near: Double, far: Double): Frustum = {
    require(focalLength > 0.0, "focal length must be strictly positive")
    require(aspect > 0.0, "aspect must be positive")
    val height = math.abs(near) / focalLength
    val width = aspect * height
    Frustum(-width / 2, width / 2, -height / 2, height / 2, near, far)
  }

  /**
   * construct frustum from focal length and sensor size
   *
   * @param focalLength focal length of lens
   * @param sensorSize  sensor size, same units as focalLength, usually mm
   * @param near        near plane (absolute value)
   * @param far         far plane (absolute value)
   */
  def fromFocalWithSensor(focalLength: Double, sensorSize: EuclideanVector[_2D], near: Double, far: Double): Frustum = {
    require(sensorSize.x > 0.0 && sensorSize.y > 0.0, "sensor size must be positive")
    fromFocal(focalLength / sensorSize.y, sensorSize.x / sensorSize.y, near, far)
  }

  /** construct frustum from a given height and aspect (w/h) */
  def fromHeight(height: Double, aspect: Double, near: Double, far: Double): Frustum = {
    require(height > 0.0, "height must be positive")
    require(aspect > 0.0, "aspect must be positive")
    val width = height * aspect
    Frustum(-width / 2, width / 2, -height / 2, height / 2, near, far)
  }
}
