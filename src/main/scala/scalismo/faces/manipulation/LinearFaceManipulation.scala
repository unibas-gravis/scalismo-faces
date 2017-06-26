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

package scalismo.faces.manipulation

import scalismo.faces.color._
import scalismo.faces.mesh.VertexColorMesh3D
import scalismo.faces.momo.{MoMo, MoMoCoefficients}
import scalismo.geometry._
import scalismo.mesh.{SurfacePointProperty, TriangleMesh3D}

/** methods for linear manipulation of Morphable Model instances */
object LinearFaceManipulation {
  /** linearly manipulate shape, point sequence */
  def manipulateShape(shape: IndexedSeq[Point[_3D]], model: MoMo, vector: MoMoCoefficients, strength: Double): IndexedSeq[Point[_3D]] = {
    require(shape.size == model.mean.shape.pointSet.numberOfPoints, "(shape) can only manipulate meshes compatible with the model")
    // get manipulated mean
    val mean = model.mean
    val manipulatedMean = model.instance(vector)
    // extract difference
    val diff = mean.shape.pointSet.points.zip(manipulatedMean.shape.pointSet.points).map { case (o, m) => m - o }.toIndexedSeq
    // apply to shape
    shape.zip(diff).map { case (s, d) => s + strength *: d }
  }

  /** linearly manipulate color, color sequence */
  def manipulateColor(color: IndexedSeq[RGBA], model: MoMo, vector: MoMoCoefficients, strength: Double): IndexedSeq[RGBA] = {
    require(color.size == model.mean.shape.pointSet.numberOfPoints, "(color) can only manipulate meshes compatible with the model")
    // get manipulated mean
    val mean = model.mean
    val manipulatedMean = model.instance(vector)
    // extract difference
    val diff = for (i <- mean.color.triangulation.pointIds) yield manipulatedMean.color.atPoint(i) - mean.color.atPoint(i)
    // apply to color
    for (i <- color.indices) yield color(i) + strength *: diff(i)
  }

  /** linearly manipulate colored mesh, applies shape and color manipulation */
  def manipulateColorMesh(mesh: VertexColorMesh3D, model: MoMo, vector: MoMoCoefficients, strengthShape: Double, strengthColor: Double): VertexColorMesh3D = {
    val triangulation = mesh.shape.triangulation
    val manipulatedColor = manipulateColor(mesh.color.pointData, model, vector, strengthColor)

    VertexColorMesh3D(
      manipulateShapeMesh(mesh.shape, model, vector, strengthShape),
      SurfacePointProperty(triangulation, manipulatedColor))
  }

  /** linearly manipulate shape mesh */
  def manipulateShapeMesh(mesh: TriangleMesh3D, model: MoMo, vector: MoMoCoefficients, strengthShape: Double): TriangleMesh3D = {
    val triangulation = mesh.triangulation
    val manipulatedShape = manipulateShape(mesh.pointSet.points.map(p => p: Point[_3D]).toIndexedSeq, model, vector, strengthShape)

    TriangleMesh3D(manipulatedShape, triangulation)
  }
}