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

package scalismo.faces

import java.net.URI

import breeze.linalg.{DenseMatrix, DenseVector, qr}
import org.scalatest._
import scalismo.common.PointId
import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.image.PixelImage
import scalismo.faces.mesh.{ColorNormalMesh3D, TextureMappedProperty, VertexColorMesh3D}
import scalismo.faces.momo.{MoMo, PancakeDLRGP}
import scalismo.geometry.{Landmark, Point, Vector, Vector3D, _2D, _3D}
import scalismo.mesh.{SurfacePointProperty, TriangleCell, TriangleList, TriangleMesh3D}
import scalismo.statisticalmodel.ModelHelpers
import scalismo.utils.Random

import scala.collection.mutable.ArrayBuffer

class FacesTestSuite extends FunSpec with Matchers {
  scalismo.initialize()

  implicit val rnd = Random(43)

  def randomRGB = RGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomRGBA = RGBA(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomVector3D = Vector3D(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomDouble: Double = rnd.scalaRandom.nextDouble()

  def randomImage(w: Int, h: Int): PixelImage[RGB] = PixelImage(w, h, (x, y) => randomRGB)

  def randomInt(max: Int): Int = rnd.scalaRandom.nextInt(max)

  def randomDirection: Vector[_2D] = Vector.fromPolar(1.0, rnd.scalaRandom.nextDouble() * 2.0 * math.Pi)

  def randomGridMesh(cols: Int = 3, rows: Int = 3, sdev: Double = 0.25): VertexColorMesh3D = {
    val n = cols * rows

    val points = IndexedSeq.tabulate(n)(i => Point(i % cols, i / cols, 0f)).map(p => p + Vector(rnd.scalaRandom.nextGaussian(), rnd.scalaRandom.nextGaussian(), rnd.scalaRandom.nextGaussian()) * sdev)
    val colors = IndexedSeq.fill(n)(randomRGBA)
    // mesh structure
    val triangles = new ArrayBuffer[TriangleCell](n * 2)
    for (i <- 0 until n) {
      val c = i % cols
      val r = i / cols
      if (c < cols - 1 && i < n - cols - 1) {
        triangles += TriangleCell(PointId(i), PointId(i + 1), PointId(i + cols))
        triangles += TriangleCell(PointId(i + cols), PointId(i + 1), PointId(i + cols + 1))
      }
    }
    val triangleList = TriangleList(triangles.toIndexedSeq)
    // mesh
    VertexColorMesh3D(
      TriangleMesh3D(points, triangleList),
      SurfacePointProperty(triangleList, colors)
    )
  }

  def randomGridMeshWithTexture(cols: Int = 3, rows: Int = 3, sdev: Double = 0.25): ColorNormalMesh3D = {
    val vcMesh = randomGridMesh(cols, rows, sdev)
    val centerPoints: IndexedSeq[Point[_2D]] = IndexedSeq.tabulate(cols*rows)(i => Point(i % cols, i / cols))
    val uvCoords: IndexedSeq[Point[_2D]] = centerPoints.map{ pt => TextureMappedProperty.imageCoordinatesToUV(pt, cols, rows)}

    val texMap: SurfacePointProperty[Point[_2D]] = SurfacePointProperty(vcMesh.shape.triangulation, uvCoords)
    val texture = TextureMappedProperty(vcMesh.shape.triangulation, texMap, PixelImage(cols, rows, (x, y) => randomRGBA))
    ColorNormalMesh3D(
      vcMesh.shape,
      texture,
      vcMesh.shape.vertexNormals
    )
  }


  def randomString(length: Int): String = rnd.scalaRandom.alphanumeric.take(length).mkString

  def randomURI(length: Int): URI = {
    val string = randomString(length).filter{_.isLetterOrDigit}
    new URI(string)
  }

  def randomGridModel(rank: Int = 10, sdev: Double = 5.0, noise: Double = 0.05, cols: Int = 5, rows: Int = 5, orthogonalExpressions: Boolean = false): MoMo = {

    val reference = randomGridMesh(cols, rows)

    val compN = 3 * cols * rows
    val fullShapeComponents = qr(DenseMatrix.fill[Double](compN, 2 * rank)(rnd.scalaRandom.nextGaussian)).q

    val shapeN = compN
    val shapeMean = DenseVector.fill[Double](shapeN)(rnd.scalaRandom.nextGaussian)
    val shapePCABases = fullShapeComponents(::, 0 until rank)
    val shapeVariance = DenseVector.fill[Double](rank)(rnd.scalaRandom.nextDouble * sdev)
    assert(shapeMean.length == shapePCABases.rows, "rows is not correct")
    assert(shapePCABases.cols == rank, "model is of incorrect rank")
    assert(shapeVariance.length == shapePCABases.cols, "wrong number of variances")
    val shape = ModelHelpers.buildFrom[_3D, Point[_3D]](reference.shape.pointSet, shapeMean, shapeVariance, shapePCABases)

    val colorN = compN
    val colorMean = DenseVector.fill[Double](colorN)(rnd.scalaRandom.nextGaussian)
    val colorPCABases = qr.reduced(DenseMatrix.fill[Double](colorN, rank)(rnd.scalaRandom.nextGaussian)).q
    val colorVariance = DenseVector.fill[Double](rank)(rnd.scalaRandom.nextDouble * sdev)
    assert(colorMean.length == colorPCABases.rows, "rows is not correct")
    assert(colorPCABases.cols == rank, "model is of incorrect rank")
    assert(colorVariance.length == colorPCABases.cols, "wrong number of variances")
    val color = ModelHelpers.buildFrom[_3D, RGB](reference.shape.pointSet, colorMean, colorVariance, colorPCABases)

    val expressionN = compN
    val expressionMean = DenseVector.fill[Double](expressionN)(rnd.scalaRandom.nextGaussian)
    val expressionPCABases =
      if (orthogonalExpressions)
        fullShapeComponents(::, rank until 2*rank)
      else
        qr.reduced(DenseMatrix.fill[Double](expressionN, rank)(rnd.scalaRandom.nextGaussian)).q
    val expressionVariance = DenseVector.fill[Double](rank)(rnd.scalaRandom.nextDouble * sdev)
    assert(expressionMean.length == expressionPCABases.rows, "rows is not correct")
    assert(expressionPCABases.cols == rank, "model is of incorrect rank")
    assert(expressionVariance.length == expressionPCABases.cols, "wrong number of variances")
    val expression = ModelHelpers.buildFrom[_3D, Vector[_3D]](reference.shape.pointSet, expressionMean, expressionVariance, expressionPCABases)

    def randomName = randomString(10)
    def randomPointOnRef = reference.shape.pointSet.points.toIndexedSeq(rnd.scalaRandom.nextInt(reference.shape.pointSet.numberOfPoints))
    def randomLM: Landmark[_3D] = Landmark(randomName, randomPointOnRef)
    val randomLandmarks = for(i <- 0 until 5) yield randomName -> randomLM

    MoMo(reference.shape,
      PancakeDLRGP(shape, noise),
      PancakeDLRGP(color, noise),
      PancakeDLRGP(expression, noise),
      randomLandmarks.toMap)
  }

  def randomGridModelExpress(rank: Int = 10, sdev: Double = 5.0, noise: Double = 0.05, cols: Int = 5, rows: Int = 5): PancakeDLRGP[_3D, Vector[_3D]] = {

    val reference = randomGridMesh(cols, rows)
    val expressionN = 3 * cols * rows
    val expressionMean = DenseVector.fill[Double](expressionN)(rnd.scalaRandom.nextGaussian)
    val expressionDecomposition = qr(DenseMatrix.fill[Double](expressionN, rank)(rnd.scalaRandom.nextGaussian))
    val expressionPCABases = expressionDecomposition.q
    val expressionVariance = DenseVector.fill[Double](expressionN)(rnd.scalaRandom.nextDouble * sdev)
    val expressionNoise = rnd.scalaRandom.nextDouble * noise
    PancakeDLRGP(ModelHelpers.buildFrom[_3D, Vector[_3D]](reference.shape.pointSet, expressionMean, expressionVariance, expressionPCABases))

  }
}
