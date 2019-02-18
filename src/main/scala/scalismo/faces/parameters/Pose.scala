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
import scalismo.geometry.{EuclideanVector, _3D}

/** face pose parameters */
case class Pose(scaling: Double,
                translation: EuclideanVector[_3D],
                roll: Double,
                yaw: Double,
                pitch: Double) {

  /** change scaling */
  def withScaling(scaling: Double): Pose = copy(scaling = scaling)

  /** change translation */
  def withTranslation(translation: EuclideanVector[_3D]): Pose = copy(translation = translation)

  /** change roll angle */
  def withRoll(roll: Double): Pose = copy(roll = roll)

  /** change yaw angle */
  def withYaw(yaw: Double): Pose = copy(yaw = yaw)

  /** change pitch angle */
  def withPitch(pitch: Double): Pose = copy(pitch = pitch)

  /** reset scaling */
  def noScaling: Pose = copy(scaling = 1.0)

  /** transform: usually the model transform known from computer graphics which transforms the object to world coordinates */
  def transform: Affine3D = RenderTransforms.modelTransform(translation, scaling, pitch, yaw, roll)

  def rotation: Rotation3D = Rotation3D.fromEulerXYZ(pitch, yaw, roll)

  def compose(inner: Pose): Pose = {
    val scComp = scaling * inner.scaling

    val rot: Rotation3D = Rotation3D.fromEulerXYZ(pitch, yaw, roll)
    val (scPitch, scYaw, scRoll) = Rotation3D.decomposeRotationXYZ(
      rot compose Rotation3D.fromEulerXYZ(inner.pitch, inner.yaw, inner.roll))

    val tComp = translation + rot(scaling *: inner.translation)

    Pose(
      scaling = scComp,
      translation = tComp,
      roll = scRoll,
      yaw = scYaw,
      pitch = scPitch
    )
  }
}

object Pose {
  val neutral = Pose(1.0, EuclideanVector(0.0, 0.0, 0.0), 0.0, 0.0, 0.0)
  val away1m = Pose(1.0, EuclideanVector(0.0, 0.0, -1000.0), 0.0, 0.0, 0.0)

  def fromTransform3D(t: Transform3D): Option[Pose] = {
    t match {
      case r: Rotation3D =>
        val (pitch, yaw, roll) = Rotation3D.decomposeRotationXYZ(r)
        Some(Pose.neutral.copy(pitch = pitch, yaw = yaw, roll = roll))
      case t: Translation3D =>
        Some(Pose.neutral.copy(translation = t.t))
      case Scaling3D(sx, sy, sz) if sx == sy && sx == sz =>
        Some(Pose.neutral.copy(scaling = sx))
      case _ => None
    }
  }
}
