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

package scalismo.faces.parameters

import scalismo.faces.render._
import scalismo.geometry._

/** camera parameterization */
case class Camera(focalLength: Double,
                  principalPoint: Point[_2D],
                  sensorSize: EuclideanVector[_2D],
                  near: Double,
                  far: Double,
                  orthographic: Boolean) {

  require(focalLength > 0.0, "focal length must be positive")
  require(sensorSize.x > 0.0 && sensorSize.y > 0.0, "sensor size must be positive")
  require(near < far, "near plane must be closer than far plane")

  /** projection of this camera, ignores principalPoint, no offset (wrong units, PP is in pixels!) */
  def projection: FrustumProjection with Transform4x4 = {
    if (orthographic)
      FrustumOrthographicProjection(Frustum.fromFocalWithSensor(focalLength, sensorSize, near, far).withCenter(principalPoint))
    else
      FrustumPinholeProjection(Frustum.fromFocalWithSensor(focalLength, sensorSize, near, far).withCenter(principalPoint))
  }

  /** viewing frustum of this scene */
  def frustum: Frustum = Frustum.fromFocalWithSensor(focalLength, sensorSize, near, far).withCenter(principalPoint)
}

object Camera {
  val sensor35mm = EuclideanVector(36, 24)

  def for35mmFilm(focalLength: Double) = Camera(focalLength, Point2D.origin, sensor35mm, orthographic = false, near = 10, far = 1000e3)
}
