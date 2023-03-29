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

import scalismo.faces.render.{Affine3D, RenderTransforms}
import scalismo.geometry.{_3D, EuclideanVector, Point, Point3D}

/** parameters describing the view setup (camera transform) */
case class ViewParameter(translation: EuclideanVector[_3D], pitch: Double, yaw: Double, roll: Double) {

  /** model transform: object to world coordinates */
  def viewTransform: Affine3D = RenderTransforms.viewTransform(translation, pitch = pitch, yaw = yaw, roll = roll)

  /** camera origin in world coordinate system */
  def eyePosition: Point[_3D] = Point3D.origin + translation

  /** camera pose in the world */
  def cameraPose = Pose(1.0, translation = translation, pitch = pitch, yaw = yaw, roll = roll)
}

object ViewParameter {
  val neutral = ViewParameter(EuclideanVector(0f, 0f, 0f), 0f, 0f, 0f)
  val away1m = ViewParameter(EuclideanVector(0f, 0f, 1000f), 0f, 0f, 0f)

  def fromPose(pose: Pose): ViewParameter =
    ViewParameter(pose.translation, pitch = pose.pitch, yaw = pose.yaw, roll = pose.roll)
}
