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

package scalismo.faces.momo

import scalismo.color.RGBA
import scalismo.mesh.VertexColorMesh3D
import scalismo.geometry.{_3D, Point}
import scalismo.utils.Random

import scala.util.Try

object ModelMetrics {

  /*
   * The implementation of these metrices is inspired from :
   * Styner, Martin A., et al. "Evaluation of 3D correspondence methods for model building." Information processing in medical imaging. Springer Berlin Heidelberg, 2003.
   *
   * an Implementation for the shape part only can also be found in scalismo/src/main/scala/scalismo/statisticalmodel/dataset/ModelMetrics.scala
   *
   *
   * For simplicity here its only implemented for data in correspondence (it would be possible only for the specificity to implement it without correspondence using closestpoints)
   *
   * Specificity:
   * how close the model remains to the category of shapes it is supposed to represent
   * The general idea is as follows :
   * 1 - sample a shape from the mesh model
   * 2-  compute the average mesh distance  of the sample to all elements of the given sequence of meshes and select the minimum distance
   * These steps are then repeated nbSamples times and the average value is returned.
   *
   * Generalization:
   * how well the model can represent unseen data
   * For every mesh in the test data, we project the mesh into the model (that is find the closest shape in the model space to the given mesh) and compute the average mesh distance between the mesh and the projection.
   * To be able to perform the projection, it is important that the data collection is in correspondence with the model.
   * The returned value is a scala.util.Try containing the average over all test data in case of success, or an Exception otherwise
   */

  /**
   * calculates average distance between two color sequences uses correspondence information
   */
  def colorDistance(a: IndexedSeq[RGBA], b: IndexedSeq[RGBA]): Try[Double] = Try {
    require(a.length == b.length, "color sequences have to have same length")
    a.zip(b).map { case (aa, bb) => (aa.toRGB - bb.toRGB).norm }.sum / a.length
  }

  /**
   * calculates average distance between two point sequences uses correspondence information (differs from scalismo
   * implementation where closestpoint is chosen
   */
  def shapeDistance(a: IndexedSeq[Point[_3D]], b: IndexedSeq[Point[_3D]]): Try[Double] = Try {
    require(a.length == b.length, "shape sequences have to have same length")
    a.zip(b).map { case (aa, bb) => (aa - bb).norm }.sum / a.length
  }

  /**
   * Returns the specificity metric of the color part of the Momo, that is how close the model remains to the category
   * of shapes it is supposed to represent works on data in correspondence only
   */
  def colorSpecificity(model: MoMo, data: Seq[VertexColorMesh3D], nbSamples: Int)(implicit rnd: Random): Try[Double] =
    Try {

      require(
        data.forall(f => f.color.triangulation.pointIds.size == model.referenceMesh.triangulation.pointIds.size),
        "reference and all meshes have to have same color length"
      )

      def minDistanceToRandomSample(): Double = {
        val sample = model.sample()
        data.map { m =>
          colorDistance(sample.color.pointData, m.color.pointData).get
        }.min
      }

      val addedErrors = IndexedSeq.fill(nbSamples)(minDistanceToRandomSample())
      addedErrors.sum / addedErrors.length
    }

  /**
   * Returns the generalization metric of the color part, that is how well the model can represent unseen data works on
   * data in correspondence only
   */
  def colorGeneralization(model: MoMo, data: Seq[VertexColorMesh3D]): Try[Double] = Try {

    require(
      data.forall(f => f.color.triangulation.pointIds.size == model.referenceMesh.triangulation.pointIds.size),
      "reference and all meshes have to have same color length"
    )

    data.map { m =>
      val projection = model.project(m)
      colorDistance(projection.color.pointData, m.color.pointData).get
    }.sum / data.length
  }

  /**
   * Returns the specificity metric of the shape part of the Momo, that is how close the model remains to the category
   * of shapes it is supposed to represent works on data in correspondence only
   */
  def shapeSpecificity(model: MoMo, data: Seq[VertexColorMesh3D], nbSamples: Int)(implicit rnd: Random): Try[Double] =
    Try {

      require(
        data.forall(f =>
          f.shape.position.triangulation.pointIds.size == model.referenceMesh.triangulation.pointIds.size
        ),
        "reference and all meshes have to have same shape length"
      )

      def minDistanceToRandomSample(): Double = {
        val sample = model.sample()
        data.map { m =>
          shapeDistance(sample.shape.pointSet.points.toIndexedSeq, m.shape.pointSet.points.toIndexedSeq).get
        }.min
      }

      val addedErrors = IndexedSeq.fill(nbSamples)(minDistanceToRandomSample())
      addedErrors.sum / addedErrors.length
    }

  /**
   * Returns the generalization metric of the shape part, that is how well the model can represent unseen data works on
   * data in correspondence only
   */
  def shapeGeneralization(model: MoMo, data: Seq[VertexColorMesh3D]): Try[Double] = Try {

    require(
      data.forall(f => f.shape.pointSet.numberOfPoints == model.referenceMesh.pointSet.numberOfPoints),
      "reference and all meshes have to have same shape length"
    )

    data.map { m =>
      val projection = model.project(m)
      shapeDistance(projection.shape.pointSet.points.toIndexedSeq, m.shape.pointSet.points.toIndexedSeq).get
    }.sum / data.length
  }

}
