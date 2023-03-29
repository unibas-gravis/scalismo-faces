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

import java.io.*
import breeze.linalg.{DenseVector, norm, sum}
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import breeze.stats.distributions.Gaussian
import scalismo.faces.FacesTestSuite
import scalismo.faces.io.MoMoIO
import scalismo.mesh.VertexColorMesh3D

import scala.annotation.unused
import scala.util.Try

class MoMoTests extends FacesTestSuite {

  def meshDist(mesh1: VertexColorMesh3D, mesh2: VertexColorMesh3D): Double = {
    val shp1: DenseVector[Double] = DenseVector(mesh1.shape.pointSet.points.toIndexedSeq.flatMap(p => IndexedSeq(p.x, p.y, p.z)).toArray)
    val shp2: DenseVector[Double] = DenseVector(mesh2.shape.pointSet.points.toIndexedSeq.flatMap(p => IndexedSeq(p.x, p.y, p.z)).toArray)
    val col1: DenseVector[Double] = DenseVector(mesh1.color.pointData.flatMap(p => IndexedSeq(p.r, p.g, p.b)).toArray)
    val col2: DenseVector[Double] = DenseVector(mesh2.color.pointData.flatMap(p => IndexedSeq(p.r, p.g, p.b)).toArray)
    val shapeDistSq = sum((shp1 - shp2).map(v => v * v))
    val colorDistSq = sum((col1 - col2).map(v => v * v))
    math.sqrt(shapeDistSq + colorDistSq)
  }

  def coeffsDist(coeffs1: MoMoCoefficients, coeffs2: MoMoCoefficients): Double = {
    require(coeffs1.shape.length == coeffs2.shape.length)
    require(coeffs1.color.length == coeffs2.color.length)
    require(coeffs1.expression.length == coeffs2.expression.length)
    norm(coeffs1.shape - coeffs2.shape) + norm(coeffs1.color - coeffs2.color) + norm(coeffs1.expression - coeffs2.expression)
  }

  describe("A MoMo") {

    // Build random PCA model for projection tests and write it to disk
    lazy val randomMomo = randomGridModel(10, 5, 0.1)
    val rf = File.createTempFile("momo-gravismomoio-test", ".h5.json")
    rf.deleteOnExit()
    MoMoIO.write(randomMomo, rf)

    // load hdf5 default model
    lazy val fullMoMo = MoMoIO.read(new File(getClass.getResource("/random-momo.h5").getPath)).get.expressionModel.get
    lazy val fullShapeCoeffs = for (_ <- 0 until fullMoMo.shape.rank) yield Gaussian(0, 1).draw()
    lazy val fullColorCoeffs = for (_ <- 0 until fullMoMo.color.rank) yield Gaussian(0, 1).draw()
    lazy val fullExpressCoeffs = for (_ <- 0 until fullMoMo.expression.rank) yield Gaussian(0, 1).draw()
    lazy val fullMoMoCoeffs = MoMoCoefficients(fullShapeCoeffs, fullColorCoeffs, fullExpressCoeffs)

    @unused
    lazy val fullSample = fullMoMo.instance(fullMoMoCoeffs)

    // PCA model needed for projection tests
    lazy val momo = MoMoIO.read(rf).get.expressionModel.get

    val momoPCA = MoMo(
      momo.referenceMesh,
      PancakeDLRGP(momo.shape.gpModel),
      PancakeDLRGP(momo.color.gpModel),
      PancakeDLRGP(momo.expression.gpModel),
      momo.landmarks)

    lazy val shapeCoeffs = for (_ <- 0 until momo.shape.rank) yield Gaussian(0, 1).draw()
    lazy val colorCoeffs = for (_ <- 0 until momo.color.rank) yield Gaussian(0, 1).draw()
    lazy val expressCoeffs = for (_ <- 0 until momo.expression.rank) yield Gaussian(0, 1).draw()
    lazy val momoCoeffs = MoMoCoefficients(shapeCoeffs, colorCoeffs, expressCoeffs)
    lazy val momoCoeffsNoEx = momoCoeffs.copy(expression = DenseVector.zeros[Double](0))
    lazy val sample = momo.instance(momoCoeffs)
    lazy val sampleNoEx = momo.neutralModel.instance(momoCoeffsNoEx)

    val distThres = 0.01 * sample.shape.pointSet.numberOfPoints + 0.01 * sample.color.triangulation.pointIds.size

    it("can load from disk") {
      momo.shape.rank should be > 0
    }

    it("should create shape samples") {
      sample.shape.pointSet.numberOfPoints should be > 0
    }

    it("should create color samples") {
      sample.color.triangulation.pointIds.size should be > 0
    }

    it("can generate random samples") {
      momo.sample().shape.triangulation.pointIds should be (momo.referenceMesh.triangulation.pointIds)
    }

    it("can generate random samples (PCA)") {
      momoPCA.sample().shape.triangulation.pointIds should be (momoPCA.referenceMesh.triangulation.pointIds)
    }

    it("can create samples with fewer parameters set") {
      val momoRed = momoCoeffs.copy(
        shape = DenseVector(momoCoeffs.shape.toArray.take(1)),
        color = DenseVector(momoCoeffs.color.toArray.take(1)),
        expression = DenseVector(momoCoeffs.expression.toArray.take(1))
      )
      val sample = momo.instance(momoRed)
      val momoRedFull = momoCoeffs.copy(
        shape = DenseVector(momoCoeffs.shape.toArray.take(1) ++ Array.fill(momo.shape.rank - 1)(0.0)),
        color = DenseVector(momoCoeffs.color.toArray.take(1) ++ Array.fill(momo.shape.rank - 1)(0.0)),
        expression = DenseVector(momoCoeffs.expression.toArray.take(1) ++ Array.fill(momo.expression.rank - 1)(0.0))
      )
      val sampleFull = momo.instance(momoRedFull)
      meshDist(sample, sampleFull) should be < distThres
    }

    it("should creates instances identical to underlying GP model (no noise on instance)") {
      val instPCA = momoPCA.instance(momoCoeffs)
      meshDist(instPCA, sample) should be < distThres
    }

    it("should not alter a sample through projection (PCA model only, no expressions)") {
      val projected = momoPCA.neutralModel.project(sampleNoEx)
      meshDist(projected, sampleNoEx) should be < distThres
    }

    it("should not alter a sample through projection (PCA model only, with expression)") {
      val projected = momoPCA.project(sample)
      meshDist(projected, sample) should be < distThres
    }

    it("should regularize a sample through projection (closer to mean, no expression)") {
      val projected = momo.neutralModel.project(sampleNoEx)
      meshDist(projected, momo.neutralModel.mean) should be < meshDist(sampleNoEx, momo.neutralModel.mean)
    }

    it("should regularize the coefficients of a sample (closer to mean)") {
      val projCoeffs = momo.coefficients(sample)
      val projCoeffPCA = momoPCA.coefficients(sample)
      norm(projCoeffs.shape) + norm(projCoeffs.expression) should be < (norm(projCoeffPCA.shape) + norm(projCoeffPCA.expression))
    }

    it("should regularize the coefficients of a sample of the neutral model (closer to mean)") {
      val projCoeffs = momo.neutralModel.coefficients(sampleNoEx)
      val projCoeffPCA = momoPCA.neutralModel.coefficients(sampleNoEx)
      norm(projCoeffs.shape) should be < norm(projCoeffPCA.shape)
    }

    it("should yield the same shape coefficients used to draw a sample (PCA model only)") {
      val projCoeffs = momoPCA.coefficients(sample)
      @unused
      val pC = momo.coefficients(sample)
      norm(projCoeffs.shape - momoCoeffs.shape) should be < 0.01 * momoCoeffs.shape.length
      norm(projCoeffs.color - momoCoeffsNoEx.color) should be < 0.01 * momoCoeffs.color.length
      norm(projCoeffs.expression - momoCoeffs.expression) should be < 0.01 * momoCoeffs.expression.length
    }

    it("should yield proper coefficients for the mean sample") {
      val meanSample = momo.mean
      val meanCoeffs = momo.coefficients(meanSample)
      norm(meanCoeffs.shape) + norm(meanCoeffs.color) should be < 0.01 * momoCoeffs.shape.length + 0.01 * momoCoeffs.color.length
    }

    val f = File.createTempFile("momo", ".h5.json")
    f.deleteOnExit()
    MoMoIO.write(momo, f).get
    val loadedMomo = MoMoIO.read(f).get.expressionModel.get

    describe("should save to and load from disk unaltered") {
      it("reference") {
        loadedMomo.referenceMesh should be(momo.referenceMesh)
      }

      it("shape") {
        loadedMomo.shape should be(momo.shape)
      }

      it("color") {
        loadedMomo.color should be(momo.color)
      }

      it("landmarks") {
        loadedMomo.landmarks should be(momo.landmarks)
      }

      it("complete MoMo") {
        loadedMomo should be(momo)
      }
    }

    it("does throw an informative message when trying to load a model built in c++") {
      val result = MoMoIO.read(new File(getClass.getResource("/random-l4.h5").getPath))
      require(result.isFailure)
      require(result.failed.get.isInstanceOf[IllegalArgumentException])
      require(result.failed.get.getMessage.contains("Probably you tried to read a legacy model file. Please convert the datatype of the cells data to the type int."))
    }

    lazy val reducedMomo = momo.truncate(5, 5, 5)
    lazy val reducedShapeCoeffs = for (_ <- 0 until reducedMomo.shape.rank) yield Gaussian(0, 1).draw()
    lazy val reducedColorCoeffs = for (_ <- 0 until reducedMomo.color.rank) yield Gaussian(0, 1).draw()
    lazy val reducedExpressCoeffs = for (_ <- 0 until reducedMomo.expression.rank) yield Gaussian(0, 1).draw()
    lazy val reducedMomoCoeffs = MoMoCoefficients(reducedShapeCoeffs, reducedColorCoeffs, reducedExpressCoeffs)
    @unused
    lazy val reducedMomoCoeffsNoEx = MoMoCoefficients(reducedShapeCoeffs, reducedColorCoeffs, IndexedSeq.empty)
    lazy val reducedSample = reducedMomo.instance(reducedMomoCoeffs)
    @unused
    lazy val reducedSampleNoEx = reducedMomo.neutralModel.instance(reducedMomoCoeffs)


    it("can be reduced to 5 shape components") {
      reducedMomo.shape.rank should be(5)
    }

    it("can be reduced to 5 color components") {
      reducedMomo.color.rank should be(5)
    }

    it("can be reduced to 5 expression components") {
      reducedMomo.expression.rank should be(5)
    }

    it("should create shape samples (reduced)") {
      reducedSample.shape.pointSet.numberOfPoints should be > 0
    }

    it("should create color samples (reduced)") {
      reducedSample.color.pointData.length should be > 0
    }

    it("should not alter a sample through projection (reduced)") {
      val projected = reducedMomo.project(reducedSample)
      meshDist(projected, reducedSample) should be < 0.1 * reducedSample.shape.pointSet.numberOfPoints + 0.01 * reducedSample.color.pointData.length
    }

//    it("should yield the same coefficients used to draw a sample (reduced)") {
//      val projCoeffs = reducedMomo.coefficients(reducedSample)
//      coeffsDist(projCoeffs, reducedMomoCoeffs) should be < 0.01 * reducedMomoCoeffs.shape.length + 0.01 * reducedMomoCoeffs.color.length + 0.01 * reducedMomoCoeffs.expression.length
//    }

    it("can be written to disk and be read again (and be equal)") {
      val f = File.createTempFile("reduced-model", ".h5.json")
      f.deleteOnExit()
      MoMoIO.write(momo, f).get
      val loadedMomo = MoMoIO.read(f).get
      loadedMomo should be(momo)
    }

    it("supports loading using an URI") {
      val modelFile = new File(getClass.getResource("/random-momo.h5").getPath)
      val momo = MoMoIO.read(modelFile, "").get
      val uri = modelFile.toURI
      val uriMomo = MoMoIO.read(uri).get
      uriMomo shouldBe momo
    }

    it("supports cached loading using an URI") {
      val uri = new File(getClass.getResource("/random-momo.h5").getPath).toURI
      val uriMomo1 = MoMoIO.read(uri).get
      val uriMomo2 = MoMoIO.read(uri).get
      // ensure same model instance: caching serves the identical instance twice
      assert(uriMomo1 eq uriMomo2)
    }

    it("it prevents drawing an instance with an empty parameter vector") {
      val emptyVector = MoMoCoefficients(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
      intercept[IllegalArgumentException](momo.instance(emptyVector))
    }

    it("can handle an empty expression model") {
      val neutralCoeffs = momoCoeffs.copy(expression = DenseVector.zeros(0))
      noException should be thrownBy momo.neutralModel.instance(neutralCoeffs)
    }
  }
}