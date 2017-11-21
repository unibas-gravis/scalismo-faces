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

import breeze.linalg.{DenseMatrix, DenseVector, min, norm}
import breeze.numerics.abs
import scalismo.common.{DiscreteField, PointId, UnstructuredPointsDomain}
import scalismo.faces.FacesTestSuite
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, ModelHelpers}

class ModelHelperTests extends FacesTestSuite {

  def vectorsNearlyEqual(A: DenseVector[Double], B: DenseVector[Double], threshold: Double = 1E-6) {
    A.length shouldBe B.length

    for(i <- 0 until A.length)
      A(i) should be (B(i) +- threshold)
  }

  def matrixColsNearlyEqualUpToSign(A: DenseMatrix[Double], B: DenseMatrix[Double], threshold: Double = 1E-6) {
    A.cols shouldBe B.cols
    A.rows shouldBe B.rows

    for(j <- 0 until A.cols) {
      val s = if( abs(A(0,j) - B(0,j)) < 1.0e-8 ) {
        1.0
      } else {
        if ( abs(A(0,j)+B(0,j)) < 1.0e-8 ) {
          -1.0
        } else {
          false shouldBe true
          0.0
        }
      }
      for (i <- 0 until A.rows) {
        A(i, j) should be ( s*B(i,j) +- threshold )
      }
    }
  }

  def matrixHasOrthonormalCols( X: DenseMatrix[Double], threshold: Double = 1E-6): Unit = {
    for( i <- 0 until X.cols) {
      val col = X(::,i)
      norm(col) shouldBe 1.0 +- threshold
      for ( j <- 0 until X.cols) {
        if ( i != j ) {
          val c2 = X(::,j)
          col.dot(c2) shouldBe 0.0 +- threshold
        }
      }
    }
  }

  def pointsAreNearlyEqual(pointFromDeformationSample: IndexedSeq[Point[_3D]],
                           pointSampled: IndexedSeq[Point[_3D]],
                           threshold: Double): Unit = {
    pointFromDeformationSample.zip(pointSampled).foreach{ case (d, p) =>
      vectorsNearlyEqual(d.toBreezeVector,p.toBreezeVector,threshold)
    }
  }

  describe("Model building") {

    class Fixture {
      val cols = 3 + rnd.scalaRandom.nextInt(20)
      val rows = 3 + rnd.scalaRandom.nextInt(20)

      val reference = randomGridMesh(cols,rows)

      val trainingMeshes = (0 until (3 + rnd.scalaRandom.nextInt(50))).map( _ => randomGridMesh(cols,rows) )

      val trainingFields = trainingMeshes.map { mesh =>
        DiscreteField[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]](reference.shape.pointSet,mesh.shape.pointSet.points.zip(reference.shape.pointSet.points).map(a => a._2 - a._1).toIndexedSeq)
      }

      val scalismoModel = DiscreteLowRankGaussianProcess.createUsingPCA[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]](reference.shape.pointSet,trainingFields.map(_.interpolateNearestNeighbor()))
      val facesModel = ModelHelpers.createUsingPCA(reference.shape.pointSet,trainingFields)
    }

    it("using our PCA method should lead to the same rank than scalismo") {
      // @note scalismo does not reduce rank but sets zero. we need to check here.
      (0 until 10) foreach { _ =>
        val f = new Fixture
        val scalismo = f.scalismoModel
        val faces = f.facesModel
        faces.rank should be <= scalismo.rank
        faces.rank shouldBe scalismo.variance.toArray.count(_>=1.0e-8)
      }
    }

    it("using our PCA method should lead to the same variances than scalismo") {
      (0 until 10) foreach { _ =>
        val f = new Fixture
        val scalismo = f.scalismoModel.variance
        val faces = f.facesModel.variance
        val len = min(scalismo.length,faces.length)
        (faces(0 until len),scalismo(0 until len))
      }
    }

    it("using our PCA method should lead to the same basis than scalismo") {
      (0 until 10) foreach { _ =>
        val f = new Fixture
        val scalismo = f.scalismoModel.basisMatrix
        val faces = f.facesModel.basisMatrix
        val rows = min(scalismo.rows,faces.rows)
        val cols = min(scalismo.cols,faces.cols)
        matrixColsNearlyEqualUpToSign(faces(0 until rows, 0 until cols),scalismo(0 until rows, 0 until cols))
      }
    }

    it("a point model built from a deformation model should lead to the same shape given parameters") {
      val f = new Fixture
      val reference  = f.reference.shape
      val deformationModel = f.facesModel
      val pointModel = ModelHelpers.vectorToPointDLRGP(deformationModel,reference)

      val sample: DiscreteField[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]] = deformationModel.sample()
      val coeffs: DenseVector[Double] = deformationModel.coefficients(sample)

      val pointSample = pointModel.instance(coeffs)
      val pointsFromPointSample = pointSample.data
      val deformationSample = deformationModel.instance(coeffs)
      val pointsFromDeformationSample = reference.pointSet.pointsWithId.map{ case (p:Point[_3D],id: PointId) =>
        p+deformationSample(id)
      }.toIndexedSeq

      pointsAreNearlyEqual(pointsFromDeformationSample,pointsFromPointSample,1.0E-6)
    }


    it("a deformation model built from a point model should lead to the same shape given parameters") {
      val f = new Fixture
      val reference  = f.reference.shape
      val pointModel = {
        val defModel = f.facesModel
        ModelHelpers.vectorToPointDLRGP(defModel,reference)
      }
      val deformationModel = ModelHelpers.pointToVectorDLRGP(pointModel,reference)

      val coeffs: DenseVector[Double] = {
        val sample: DiscreteField[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]] = deformationModel.sample()
        deformationModel.coefficients(sample)
      }

      val pointSample = pointModel.instance(coeffs)
      val pointsFromPointSample = pointSample.data
      val deformationSample = deformationModel.instance(coeffs)
      val pointsFromDeformationSample = reference.pointSet.pointsWithId.map{ case (p:Point[_3D],id: PointId) =>
        p+deformationSample(id)
      }.toIndexedSeq

      pointsAreNearlyEqual(pointsFromDeformationSample,pointsFromPointSample,1.0E-6)
    }

  }


  describe("Matrix decomposition") {

    object Fixtures {
      val tests = (0 until 10) map (_ => new Fixture)
    }

    class Fixture {
      val m = 10 + rnd.scalaRandom.nextInt(100) // #-samples
      val n = 10 + rnd.scalaRandom.nextInt(100) // #-dimensions
      val R: DenseMatrix[Double] = DenseMatrix.rand[Double](m,n) // random data matrix
      val (meanfree, mean) = ModelHelpers.removeColMean(R)

      val (cEVec,cEVal) = ModelHelpers.decomposeCovarianceMatrix(meanfree)
      val (gEVec,gEVal) = ModelHelpers.decomposeGramMatrix(meanfree)
    }

    it("leads to eigenvectors with the same length than the samples") {
      val fs = Fixtures
      fs.tests.foreach(f => f.cEVec.rows == f.n)
      fs.tests.foreach(f => f.gEVec.rows == f.n)
    }

    it("gives at most n-1 eigenvectors") {
      val fs = Fixtures
      fs.tests.foreach(f => f.cEVec.cols < f.m)
      fs.tests.foreach(f => f.gEVec.cols < f.m)
    }

    it("gives at most n-1 eigenvalues") {
      val fs = Fixtures
      fs.tests.foreach(f => f.cEVal.length < f.m)
      fs.tests.foreach(f => f.gEVal.length < f.m)
    }

    it("leads to orthonormal vectors") {
      val fs = Fixtures
      fs.tests.foreach(f => matrixHasOrthonormalCols(f.gEVec) )
      fs.tests.foreach(f => matrixHasOrthonormalCols(f.cEVec) )
    }

    it("should give the same variances when using the covariance matrix or the gram matrix") {
      val fs = Fixtures
      fs.tests.foreach(f => vectorsNearlyEqual(f.cEVal,f.gEVal) )
    }

    it("should give the same principal components when using the covariance matrix or the gram matrix") {
      val fs = Fixtures
      fs.tests.foreach(f => matrixColsNearlyEqualUpToSign(f.cEVec,f.gEVec) )
    }

  }

}
