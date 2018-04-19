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
package scalismo.statisticalmodel

import breeze.linalg.svd.SVD
import breeze.linalg.{*, DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.faces.momo.{MoMo, PancakeDLRGP}
import scalismo.geometry._
import scalismo.mesh.TriangleMesh

import scala.util.{Failure, Success, Try}

object ModelHelpers {


  /**
    *  Converts a deformation model (DLRGP for Vector[_3D]) to a point distribution model (DLRGP for Point[_3D]).
    * @param model DLRGP Vector[_3D] model
    * @param reference Reference used to map the deformation model to a point model.
    * @return DLRGP Point[_3D] model
    */
  def vectorToPointDLRGP(model: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]],
                         reference: TriangleMesh[_3D])
  : DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Point[_3D]] = {
    def vectorFieldToPointField( pf: DiscreteField[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]],
                                 f: (Vector[_3D], PointId) => Point[_3D]
                               ) = new DiscreteField[_3D, UnstructuredPointsDomain[_3D], Point[_3D]](
      pf.domain,
      pf.valuesWithIds.map{ case (v,i) =>f(v, i)}.toIndexedSeq
    )

    val newKLBasis = model.klBasis.map( b =>
      DiscreteLowRankGaussianProcess.Eigenpair[_3D, UnstructuredPointsDomain[_3D], Point[_3D]](
        b.eigenvalue,
        vectorFieldToPointField( b.eigenfunction, (v: Vector[_3D], _) => v.toPoint )
      )
    )
    val newMeanField = vectorFieldToPointField(model.mean, (v: Vector[_3D], i: PointId) => reference.pointSet.point(i)+v)

    DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Point[_3D]](newMeanField, newKLBasis)
  }

  /**
    *  Converts a point distribution model (DLRGP for Point[_3D]) to a deformation model (DLRGP for Vector[_3D]).
    * @param model DLRGP Point[_3D] model
    * @param reference Reference used to map the point model to a deformation model.
    * @return DLRGP Vector[_3D] model
    */
  def pointToVectorDLRGP(model: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Point[_3D]], reference: TriangleMesh[_3D]): DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]] = {
    def pointFieldToVectorField( pf: DiscreteField[_3D, UnstructuredPointsDomain[_3D], Point[_3D]],
                                 f: (Point[_3D], PointId) => Vector[_3D]
                               ) = new DiscreteField[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]](
      pf.domain,
      pf.valuesWithIds.map{ case (v,i) =>f(v, i)}.toIndexedSeq
    )

    val newKLBasis = model.klBasis.map( b =>
      DiscreteLowRankGaussianProcess.Eigenpair[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]](
        b.eigenvalue,
        pointFieldToVectorField( b.eigenfunction, (v: Point[_3D], _) => v.toVector )
      )
    )
    val newMeanField = pointFieldToVectorField(model.mean, (p: Point[_3D], i: PointId) => p-reference.pointSet.point(i) )

    DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]](newMeanField, newKLBasis)
  }

  /**
    * Helper function to build a DLRGP. Simply provides access to the constructor.
    */
  def buildFrom[D <: Dim : NDSpace, DDomain <: DiscreteDomain[D], Value](domain: DDomain, meanVec: DenseVector[Double], d2: DenseVector[Double], U: DenseMatrix[Double])
                                          (implicit vectorizer: Vectorizer[Value])
  : DiscreteLowRankGaussianProcess[D, DDomain, Value] = {
    new DiscreteLowRankGaussianProcess[D, DDomain, Value](domain, meanVec, d2, U)
  }


  /**
    * Creates a discrete low rank GP from samples. This method assumes that the training data as DiscreteFields are in dense correspondence with the DiscreteDomain as reference.
    *
    * @param domain         The domain where the model is defined.
    * @param discreteFields The training samples.
    * @param threshold      The minimal value to keep the basis.
    * @return A discrete low rank GP learned from the samples.
    */
  def createUsingPCA[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], Value](domain: DDomain,
                                               discreteFields: Seq[DiscreteField[D, DDomain, Value]],
                                               threshold: Double = 1.0e-8)
                                               (implicit vectorizer: Vectorizer[Value])
  : DiscreteLowRankGaussianProcess[D, DDomain, Value] = {
    val X = buildDataMatrixWithSamplesInCols[D, DDomain, Value](domain, discreteFields)
    val (basis, variance, mean) = calculatePPCABasis(X,0.0,threshold)
    DiscreteLowRankGaussianProcess(domain,mean,variance,basis)
  }


  /**
    * Creates a discrete low rank GP from samples. This method assumes that the training data as DiscreteFields are in dense correspondence with the DiscreteDomain as reference.
    *
    * @param domain         The domain where the model is defined.
    * @param discreteFields The training samples.
    * @return A discrete low rank GP learned from the samples.
    */
  def createUsingPPCA[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], Value](domain: DDomain,
                                                discreteFields: Seq[DiscreteField[D, DDomain, Value]],
                                                noiseVariance: Double,
                                                threshold: Double = 1.0e-8)
                                               (implicit vectorizer: Vectorizer[Value])
  : PancakeDLRGP[D, DDomain, Value] = {

    val X = buildDataMatrixWithSamplesInCols[D, DDomain, Value](domain, discreteFields)
    val (basis, variance, mean) = calculatePPCABasis(X,noiseVariance,threshold)
    PancakeDLRGP(DiscreteLowRankGaussianProcess(domain,mean,variance,basis), noiseVariance)
  }

  /**
    * Create a masked model using a set of PointIds.
    * Please note that the procedure can return a model with less PointId's because of the maskPoints operation.
    * @param momo The model to be masked.
    * @param pointIds The PointIds of the reference to be kept.
    * @return the masked model.
    */

  def maskMoMo(momo : MoMo, pointIds: Seq[PointId], strict: Boolean = true): Try[MoMo] = {

    val referenceMesh = momo.referenceMesh
    val op = referenceMesh.operations.maskPoints(id => pointIds.contains(id))
    val maskedMesh = op.transformedMesh

    if (strict && (maskedMesh.pointSet.numberOfPoints != pointIds.distinct.size) ) {
      return Failure(new Exception(
        "Masking the model would remove additonal points not specified in the provided list of point ids.\n"+
        "Either provide a different list of point ids or set the parameter stict to false to mask the model."))
    }

    val remainingPtIds = maskedMesh.pointSet.pointIds.map(id => op.pointBackMap(id)).toIndexedSeq
    val maskedModelShape = momo.neutralModel.shape.marginal(remainingPtIds)
    val maskedModelColor = momo.neutralModel.color.marginal(remainingPtIds)

    if(momo.hasExpressions){
      val maskedModelExpressions = momo.expressionModel.get.expression.marginal(remainingPtIds)
      Success(MoMo(maskedMesh, maskedModelShape, maskedModelColor, maskedModelExpressions, momo.landmarks))
    } else {
      Success(MoMo(maskedMesh, maskedModelShape, maskedModelColor, momo.landmarks))
    }

  }

  /**
    * Create a masked model using a masked reference mesh.
    * The input maskMesh for masking needs to be a subset of the reference mesh.
    * @param momo The model to be masked.
    * @param maskMesh The masked mesh, which must be a subset of the original reference mesh. It will be the new reference mesh for the output MoMo.
    * @return the masked model.
    */

  def maskMoMo(momo : MoMo, maskMesh: TriangleMesh[_3D]): Try[MoMo] = {

    val remainingPtIds = maskMesh.pointSet.points.map(p => momo.referenceMesh.pointSet.findClosestPoint(p).id).toIndexedSeq
    val maskedReference = momo.referenceMesh.operations.maskPoints(pid => remainingPtIds.contains(pid)).transformedMesh
    if (maskMesh == maskedReference) {
      val maskedModelShape = momo.neutralModel.shape.marginal(remainingPtIds)
      val maskedModelColor = momo.neutralModel.color.marginal(remainingPtIds)

      if (momo.hasExpressions) {
        val maskedModelExpressions = momo.expressionModel.get.expression.marginal(remainingPtIds)
        Success(MoMo(maskMesh, maskedModelShape, maskedModelColor, maskedModelExpressions, momo.landmarks))
      } else {
        Success(MoMo(maskMesh, maskedModelShape, maskedModelColor, momo.landmarks))
      }
    } else {
      Failure(new Exception("The mesh you provided does not seem to be a masked version of the reference."))
    }

  }

  /**
    *
    * @param X Data matrix.
    * @param noiseVariance External estimate of the noise variance.
    * @param threshold Threshold for keeping components.
    * @return (basis,variance,mean)
    */
  def calculatePPCABasis[D <: Dim : NDSpace, DDomain <: DiscreteDomain[D], Value](X: DenseMatrix[Double],
                                                 noiseVariance: Double,
                                                 threshold: Double = 1.0e-8)
                                                (implicit vectorizer: Vectorizer[Value])
  : (DenseMatrix[Double], DenseVector[Double], DenseVector[Double]) = {
    val m = X.rows
    val n = X.cols

    val (x0, meanVec) = removeColMean(X)

    // decide what to do depending on the dimensions
    val (eVec, eVal) = if (n < m) {
      decomposeGramMatrix(x0,threshold)
    } else {
      decomposeCovarianceMatrix(x0,threshold)
    }

    val rVal = eVal.map(_ - noiseVariance)
    val rank = rVal.toArray.count(_ > threshold)

    (eVec(::, 0 until rank), rVal(0 until rank), meanVec)
  }

  /**
    * Calculate the orthonormal basis and the variances of XX' using the Gram matrix X'X.
    *
    * @return The cols of the matrix hold the orthonormal eigenvectors and the vector the eigenvalues.
    */
  def decomposeGramMatrix(X: DenseMatrix[Double], threshold: Double = 1.0e-8): (DenseMatrix[Double], DenseVector[Double]) = {
    val m = X.rows
    val n = X.cols

    val G = X.t * X

    val SVD(u, s, vt) = breeze.linalg.svd(G * (1.0 / (n-1)))

    // calculate factor for eigenvectors
    val S = s.map(t => Math.sqrt(t * n))
    val SInv = s.map(t => if (Math.sqrt(t) > 1.0e-14) 1.0 / Math.sqrt(t * (n-1)) else 0.0)

    // a Matrix with the scaled eigenvectors
    val U = X * u * breeze.linalg.diag(SInv)

    val rank = s.toArray.count(_ > threshold)

    (U(::, 0 until rank), s(0 until rank))
  }

  /**
    * Calculate the orthonormal basis and the variances of the covariance matrix XX'.
    *
    * @return The cols of the matrix hold the orthonormal eigenvectors and the vector the eigenvalues.
    */
  def decomposeCovarianceMatrix(X: DenseMatrix[Double], threshold: Double = 1.0e-8): (DenseMatrix[Double], DenseVector[Double]) = {
    val m = X.rows
    val n = X.cols

    val Cov = X * X.t * (1.0 / (n-1))

    val SVD(u, s, v) = breeze.linalg.svd(Cov)
    val rank = s.toArray.count(_ > threshold)

    (u(::, 0 until rank), s(0 until rank))
  }


  /**
    * Removes row-mean from each row.
    *
    * @param X
    * @return
    */
  def removeRowMean(X: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val X0 = X
    val m: DenseVector[Double] = breeze.stats.mean(X0(::, *)).t.toDenseVector
    for (i <- 0 until X0.rows) {
      X0(i, ::) := X0(i, ::) - m.t
    }
    (X0, m)
  }

  /**
    * Removes col-mean from each col.
    *
    * @param X
    * @return
    */
  def removeColMean(X: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val X0 = X
    val m: DenseVector[Double] = breeze.stats.mean(X0(*, ::)).toDenseVector
    for (j <- 0 until X0.cols) {
      X0(::, j) := X0(::, j) - m
    }
    (X0, m)
  }

  /**
    * Build matrix with the samples stored in rows.
    */
  def buildDataMatrixWithSamplesInRows[D <: Dim : NDSpace, DDomain <: DiscreteDomain[D], Value](domain: DiscreteDomain[D], discreteFields: Seq[DiscreteField[D, DDomain, Value]])
                                                                 (implicit vectorizer: Vectorizer[Value])
  : DenseMatrix[Double] = {

    val dim = vectorizer.dim
    val n = discreteFields.size
    val p = domain.numberOfPoints

    // create the data matrix
    val X = DenseMatrix.zeros[Double](n, p * dim)
    for (f <- discreteFields.zipWithIndex.par) {
      val i = f._2
      val field = f._1
      field.data.zipWithIndex.map { p =>
        val j = p._2
        val value = p._1
        val ux = vectorizer.vectorize(value)
        val colRange = j * dim until (j + 1) * dim
        X(i, colRange) := ux.t
      }
    }
    X
  }

  /**
    * Build matrix with the samples stored in cols.
    */
  def buildDataMatrixWithSamplesInCols[D <: Dim : NDSpace, DDomain <: DiscreteDomain[D], Value](domain: DDomain, discreteFields: Seq[DiscreteField[D, DDomain, Value]])
                                                                 (implicit vectorizer: Vectorizer[Value])
  : DenseMatrix[Double] = {

    val dim = vectorizer.dim
    val n = discreteFields.size
    val p = domain.numberOfPoints
    val m = p * dim

    // create the data matrix
    val X = DenseMatrix.zeros[Double](m, n)
    for (f <- discreteFields.zipWithIndex.par) {
      val j = f._2
      val field = f._1
      field.data.zipWithIndex.map { p =>
        val i = p._2
        val value = p._1
        val ux = vectorizer.vectorize(value)
        val rowRange = i * dim until (i + 1) * dim
        X(rowRange, j) := ux
      }
    }
    X
  }
}
