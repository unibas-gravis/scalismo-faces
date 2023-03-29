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

import breeze.linalg.{diag, DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.geometry.{Dim, NDSpace, Point}
import scalismo.kernels.{DiscreteMatrixValuedPDKernel, MatrixValuedPDKernel}
import scalismo.statisticalmodel._
import scalismo.utils.Random

import scala.language.higherKinds

/**
 * Models of the type DiscreteLowRankGaussianProcess + noiseVariance * delta - extension of DLRGP to whole space
 * (pancake) This is a PPCA model but the basis matrix does not need to be orthogonal
 *
 * @param gpModel
 *   underlying discrete low-rank Gaussian Process, subspace model
 * @param noiseVariance
 *   isotropic noise variance, "subspace + N(0, noiseVariance)"
 * @tparam D
 *   dimensionality of domain
 * @tparam Value
 *   value type of model
 */
case class PancakeDLRGP[D <: Dim: NDSpace, DDomain[A] <: DiscreteDomain[A], Value](
  gpModel: DiscreteLowRankGaussianProcess[D, DDomain, Value],
  noiseVariance: Double
) {
  require(noiseVariance >= 0.0, "noise variance cannot be negative")

  implicit val vectorizer: Vectorizer[Value] = gpModel.vectorizer

  /** output dimension of GP, same as underlying */
  val outputDim: Int = gpModel.outputDim

  /** domain where this Gaussian Process is defined (same as underling process) */
  val domain: DiscreteDomain[D] = gpModel.domain

  private val totalNoiseVariance = math.max(noiseVariance, PancakeDLRGP.numericalNoiseVariance)

  /**
   * Noise distribution, expands the low rank subspace distribution to whole domain/space
   */
  val noiseDistribution: MultivariateNormalDistribution = MultivariateNormalDistribution(
    DenseVector.zeros[Double](outputDim),
    DenseMatrix.eye[Double](outputDim) * totalNoiseVariance
  )

  /** See [[DiscreteLowRankGaussianProcess.rank]] */
  val rank: Int = gpModel.rank

  /** mean field */
  val mean: DiscreteField[D, DDomain, Value] = gpModel.mean

  /** covariance of this Gaussian Process: underlying GP + local uncorrelated noise */
  val cov: DiscreteMatrixValuedPDKernel[D] = new DiscreteMatrixValuedPDKernel[D](
    domain,
    (pt1, pt2) => gpModel.cov(pt1, pt2) + noiseDistribution.cov,
    outputDim
  )

  /** this process as a DiscreteGaussianProcess, full rank */
  def toDiscreteGaussianProcess: DiscreteGaussianProcess[D, DDomain, Value] = DiscreteGaussianProcess(mean, cov)

  /** interpolate this process with nearest neighbours */
  def interpolateNearestNeighbor: GaussianProcess[D, Value] = {
    val meanNN: Field[D, Value] =
      Field(EuclideanSpace[D], (pt: Point[D]) => mean(domain.pointSet.findClosestPoint(pt).id))
    val covNN = new MatrixValuedPDKernel[D] {
      override protected def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
        val xId = gpModel.domain.pointSet.findClosestPoint(x).id
        val yId = gpModel.domain.pointSet.findClosestPoint(y).id
        cov.k(xId, yId)
      }

      override def outputDim: Int = gpModel.outputDim

      override def domain: Domain[D] = EuclideanSpace[D]
    }

    GaussianProcess(meanNN, covNN)
  }

  // model matrices as needed by coefficients
  private val D: DenseVector[Double] = gpModel.variance
  private val S: DenseVector[Double] = breeze.numerics.sqrt(D)
  private val U: DenseMatrix[Double] = gpModel.basisMatrix
  private lazy val W: DenseMatrix[Double] = U * breeze.linalg.diag(S)

  // we cannot assume an orthonormal basis in general
  // e.g. Nystroem bases are not orthonormal w.r.t the matrix
  private lazy val Minv: DenseMatrix[Double] = {
    breeze.linalg.inv(
      ((U.t * U) * diag(S *:* S)) + (DenseMatrix.eye[Double](rank) *:* totalNoiseVariance)
    )
  }

  private val totalDim: Int = outputDim * domain.pointSet.numberOfPoints

  // defensive copies of internal matrices - wasteful!!, read-only view would be great

  /** mean vector of underlying DLRGP model */
  def meanVector: DenseVector[Double] = gpModel.meanVector.copy

  /** low-rank basis matrix, (outputdim*numberOfPoints)x(rank) */
  def basisMatrix: DenseMatrix[Double] = U.copy

  /** variance along each basis direction */
  def variance: DenseVector[Double] = D.copy

  /** standard deviation along each basis direction */
  def stddev: DenseVector[Double] = S.copy

  /** scaled basis matrix, scaled with standard deviations ("W" [PPCA lit.] or "Q" [GP lit.]) */
  def basisMatrixScaled: DenseMatrix[Double] = W.copy

  /** inverted intermediate matrix M */
  def matrixMinv: DenseMatrix[Double] = Minv.copy

  /**
   * Draw an instance from the model, instance of underlying DiscreteLowRankGaussianProcess
   */
  def instance(c: DenseVector[Double]): DiscreteField[D, DDomain, Value] = gpModel.instance(c)

  /**
   * Get instance at a specific point
   */
  def instanceAtPoint(c: DenseVector[Double], pid: PointId): Value = gpModel.instanceAtPoint(c, pid)

  /**
   * Returns the probability density of the given instance, takes spherical noise term into account
   */
  def pdf(instance: DiscreteField[D, DDomain, Value]): Double = coefficientsDistribution.pdf(coefficients(instance))

  /**
   * Returns the log of the probability density of the instance
   *
   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable, takes spherical
   * noise term into account
   */
  def logpdf(instance: DiscreteField[D, DDomain, Value]): Double =
    coefficientsDistribution.logpdf(coefficients(instance))

  /**
   * Draw a sample from the model, includes noise(!) (use underlying gpModel without noise)
   */
  def sample()(implicit rnd: Random): DiscreteField[D, DDomain, Value] = {
    val coeffs: DenseVector[Double] = coefficientsDistribution.sample()
    val gpInst = gpModel.instance(coeffs)
    DiscreteField(gpInst.domain,
                  gpInst.data.map { d => vectorizer.unvectorize(vectorizer.vectorize(d) + noiseDistribution.sample()) }
    )
  }

  /**
   * Get the probability distribution of the coefficients in the latent space of the model (multivariate standard
   * normal).
   *
   * @return
   *   MultivariateNormalDistribution(0,I).
   */
  lazy val coefficientsDistribution: MultivariateNormalDistribution =
    MultivariateNormalDistribution(DenseVector.zeros[Double](rank), DenseMatrix.eye[Double](rank))

  /**
   * Project the sample into this model, best reconstruction
   */
  def project(s: DiscreteField[D, DDomain, Value]): DiscreteField[D, DDomain, Value] = instance(coefficients(s))

  /**
   * Get the low-rank expansion coefficients of a sample, respects model noise
   */
  def coefficients(s: DiscreteField[D, DDomain, Value]): DenseVector[Double] = {
    val instanceVector = DiscreteField.vectorize(s)
    assert(instanceVector.length == totalDim)
    Minv * (diag(S) * (U.t * (instanceVector - meanVector)))
  }

  /**
   * Calculate the posterior model given point observations with a common independent isotropic Gaussian noise
   *
   * @param sigma2
   *   observation noise of sample, *additional* (independent) to model noise
   */
  def posterior(trainingData: IndexedSeq[(PointId, Value)], sigma2: Double): PancakeDLRGP[D, DDomain, Value] = {
    require(sigma2 >= 0.0)
    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim),
                                             DenseMatrix.eye[Double](outputDim) *:* (sigma2 + totalNoiseVariance)
    )
    val newtd = trainingData.map { case (ptId, df) => (ptId, df, cov) }
    posterior(newtd)
  }

  /**
   * Calculate the posterior model given point observations with an individual uncertainty for each
   *
   * @param trainingData
   *   list of point observations (PointId, Value, Uncertainty), uncertainty is *additional* (independent) to model
   *   noise
   */
  def posterior(
    trainingData: IndexedSeq[(PointId, Value, MultivariateNormalDistribution)]
  ): PancakeDLRGP[D, DDomain, Value] = {
    def addMVN(mvn1: MultivariateNormalDistribution,
               mvn2: MultivariateNormalDistribution
    ): MultivariateNormalDistribution = {
      MultivariateNormalDistribution(mvn1.mean + mvn2.mean, mvn1.cov + mvn2.cov)
    }
    val newTD = trainingData.map { case (ptId, value, uncertainty) =>
      (ptId, value, addMVN(noiseDistribution, uncertainty))
    }
    PancakeDLRGP(DiscreteLowRankGaussianProcess.regression(gpModel, newTD), noiseVariance)
  }

  /** truncate the underlying low rank GP model (drops components, does not change noise) */
  def truncate(rank: Int): PancakeDLRGP[D, DDomain, Value] = {
    PancakeDLRGP(gpModel.truncate(rank), noiseVariance)
  }
}

object PancakeDLRGP {
  val numericalNoiseVariance = 1e-15

  /** create a PancakeDLRGP with minimal numeric noise variance */
  def apply[D <: Dim: NDSpace, DDomain[A] <: DiscreteDomain[A], Value](
    gpModel: DiscreteLowRankGaussianProcess[D, DDomain, Value]
  )(implicit vec: Vectorizer[Value]) = new PancakeDLRGP(gpModel, numericalNoiseVariance)
}
