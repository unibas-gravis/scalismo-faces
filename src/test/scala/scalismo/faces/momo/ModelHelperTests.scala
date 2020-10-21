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

import breeze.linalg.{*, DenseMatrix, DenseVector, min, norm}
import breeze.numerics.abs
import org.scalactic.TolerantNumerics
import scalismo.color.RGBA
import scalismo.common.{DiscreteField, PointId, UnstructuredPoints, UnstructuredPointsDomain}
import scalismo.faces.FacesTestSuite
import scalismo.faces.mesh.BinaryMask
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.mesh.{SurfacePointProperty, TriangleMesh, VertexColorMesh3D}
import scalismo.statisticalmodel.dataset.DataCollection
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

  def matricesNearlyEqual(A: DenseMatrix[Double], B: DenseMatrix[Double], threshold: Double = 1E-6) {
    A.cols shouldBe B.cols
    A.rows shouldBe B.rows
    for(j <- 0 until A.cols) {
      for (i <- 0 until A.rows) {
        A(i, j) should be ( B(i,j) +- threshold )
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

      val DC = DataCollection.fromTriangleMesh3DSequence(reference.shape,trainingMeshes.map(_.shape))
      val scalismoModel = DiscreteLowRankGaussianProcess.createUsingPCA[_3D, TriangleMesh, EuclideanVector[_3D]](DC)

      val trainingFields = trainingMeshes.map { mesh =>
        DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]](reference.shape,mesh.shape.pointSet.points.zip(reference.shape.pointSet.points).map(a => a._2 - a._1).toIndexedSeq)
      }
      val facesModel = ModelHelpers.createUsingPCA(reference.shape,trainingFields)
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

      val sample: DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]] = deformationModel.sample()
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
        val sample: DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]] = deformationModel.sample()
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

  describe("Model masking") {

    object Fixture {
      val randomModel = randomGridModel()
      val reducedMesh = randomModel.referenceMesh.operations.mask(pid => pid.id%4!=0,_=>true).transformedMesh
    }

    it ("should fail if wrong point id sequence is given for strict mode") {
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,Seq(PointId(0),PointId(1),PointId(5),PointId(2)),strict = true).isFailure)
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,Seq(PointId(0),PointId(1),PointId(2)),strict = true).isFailure)
    }

    it ("should succeed if good point id sequence is given for strict mode") {
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,Seq(PointId(0),PointId(1),PointId(5)),strict = true).isSuccess)
    }

    it ("should succeed if wrong point id sequence is given for non-strict mode") {
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,Seq(PointId(0),PointId(1),PointId(5),PointId(2)),strict = false).isSuccess)
    }

    it ("should succeed if the passed mesh has different points than the reference mesh") {
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,Fixture.reducedMesh).isSuccess)
    }

    it ("should fail if the passed mesh has different points than the reference mesh") {
      val reducedMesh = Fixture.reducedMesh
      val distortedMesh = reducedMesh.copy(pointSet = UnstructuredPoints(reducedMesh.pointSet.points.map(pt => pt.copy(x = pt.x+1.0e-6)).toIndexedSeq))
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,distortedMesh).isFailure)
    }

    it ("should fail if the passed mesh has different triangulation than the reference mesh") {
      val reducedMesh = Fixture.reducedMesh
      val distortedMesh = reducedMesh.copy(triangulation = reducedMesh.triangulation.copy(triangles = scala.util.Random.shuffle(reducedMesh.triangulation.triangles)))
      assert(ModelHelpers.maskMoMo(Fixture.randomModel,distortedMesh).isFailure)
    }
  }


  describe("Reconstructive model") {

    class Fixture {
      val cols = 3 + rnd.scalaRandom.nextInt(20)
      val rows = 3 + rnd.scalaRandom.nextInt(20)

      val reference = randomGridMesh(cols,rows)
      val trainingMeshes = (0 until (3 + rnd.scalaRandom.nextInt(50))).map( _ => randomGridMesh(cols,rows) )

      val trainingFields = trainingMeshes.map { mesh =>
        DiscreteField(reference.shape,mesh.shape.pointSet.points.zip(reference.shape.pointSet.points).map(a => a._2 - a._1).toIndexedSeq)
      }

      val mask = {
        // we account for the fact that when reducing a mesh based of points,
        // more points can drop out if they are no longer contained in a triangle
        val proposedMask = BinaryMask(IndexedSeq.fill[Boolean](reference.shape.pointSet.numberOfPoints-2)(rnd.scalaRandom.nextDouble()>0.05):+false:+true)
        val referenceMasker = reference.shape.operations.maskPoints(proposedMask)
        val reducedReference = referenceMasker.transformedMesh
        BinaryMask.createFromMeshes(reference.shape,reducedReference)
      }

      def maskMesh(mesh: VertexColorMesh3D) = {
        val meshMasker = mesh.shape.operations.maskPoints(mask)
        VertexColorMesh3D(meshMasker.transformedMesh, meshMasker.applyToSurfaceProperty(mesh.color).asInstanceOf[SurfacePointProperty[RGBA]])
      }

      def maskField(field: DiscreteField[_3D,TriangleMesh,EuclideanVector[_3D]]) : DiscreteField[_3D,TriangleMesh,EuclideanVector[_3D]] =  {
        val maskedDomain = (field.domain).operations.maskPoints(mask).transformedMesh
        DiscreteField(maskedDomain,mask.cut(field.data))
      }

      val maskedReference = maskMesh(reference)
      val maskedMeshes = trainingMeshes.map(maskMesh)
      val maskedTrainingFields = maskedMeshes.map { mesh =>
        DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]](maskedReference.shape,mesh.shape.pointSet.points.zip(maskedReference.shape.pointSet.points).map(a => a._2 - a._1).toIndexedSeq)
      }

      val maskedModel = ModelHelpers.createUsingPCA(maskedReference.shape,maskedTrainingFields)
      val reconstructiveModel = ModelHelpers.createReconstructiveUsingPCA(reference.shape,trainingFields,mask)

    }

    it ("should return the same instance in the masked area as the masked model") {
      for (i <- 0 until 20) {
        val fixture = new Fixture()
        val masked = fixture.maskedModel
        val reconstructive = fixture.reconstructiveModel
        val coeffs = DenseVector.fill(masked.rank)(rnd.scalaRandom.nextGaussian())
        val instance = masked.instance(coeffs)
        val reconstructed = fixture.maskField(reconstructive.instance(coeffs))

        require(instance equals reconstructed)
      }
    }


    it ("should return the same as calculating the reconstruction of eigenvectors") {
      val fixture = new Fixture()
      val domain = fixture.maskedReference.shape
      val discreteFields = fixture.maskedTrainingFields
      val threshold = 1e-8

      val X = ModelHelpers.buildDataMatrixWithSamplesInCols[_3D,TriangleMesh,EuclideanVector[_3D]](domain, discreteFields)
      val (x0,xmean) = ModelHelpers.removeColMean(X)
      val (basis, variance, mean) = ModelHelpers.calculatePPCABasis(X,0.0,threshold)

      val XC = ModelHelpers.buildDataMatrixWithSamplesInCols[_3D,TriangleMesh,EuclideanVector[_3D]](fixture.reference.shape,fixture.trainingFields)
      val (xC0,xcmean) = ModelHelpers.removeColMean(XC.copy)
      val C = DenseMatrix((0 until basis.cols).map{i =>
        val col = basis(::,i).toDenseVector
        x0.toDenseMatrix \ col
      }:_*)
      val basisC = xC0 * C.t

      val M = ModelHelpers.buildColumnIndexingVectorForMask[_3D,TriangleMesh,EuclideanVector[_3D]](domain, fixture.mask)
      val (basisR, varianceR, meanR) = ModelHelpers.calculateMaskedPPCABasis(XC,M,0.0,threshold)

      val nearEquality =  TolerantNumerics.tolerantDoubleEquality(1.0e-8)

      require(xcmean.size == meanR.size,s"${xcmean.size} != ${meanR.size}")
      vectorsNearlyEqual(xcmean,meanR)

      require(basisC.rows == basisR.rows,s"${basisC.rows} != ${basisR.rows}")
      require(basisC.cols == basisR.cols,s"${basisC.cols} != ${basisR.cols}")
      matricesNearlyEqual(basisC,basisR)
    }
  }

}
