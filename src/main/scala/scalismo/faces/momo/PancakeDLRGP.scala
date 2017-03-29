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

import breeze.linalg.{DenseMatrix, DenseVector, diag}
import breeze.stats.distributions.Gaussian
import scalismo.common._
import scalismo.geometry.{Dim, NDSpace}
import scalismo.utils.Random

/**
  * Models of the type DiscreteLowRankGaussianProcess + noiseVariance * delta - extension of DLRGP to whole space (pancake)
  * This is a PPCA model but the basis matrix does not need to be orthogonal
  *
  * @param gpModel underlying discrete low-rank Gaussian Process, subspace model
  * @param noiseVariance isotropic noise variance, "subspace + N(0, noiseVariance)"
  * @tparam D dimensionality of domain
  * @tparam Value value type of model
  */
case class PancakeDLRGP[D <: Dim: NDSpace, Value](gpModel: DiscreteLowRankGaussianProcess[D, Value],
                                                  noiseVariance: Double)
                                                 (implicit vec: Vectorizer[Value])
  extends DiscreteGaussianProcess[D, Value](gpModel.mean, gpModel.cov) {

  require(noiseVariance >= 0.0, "noise variance cannot be negative")

  private val totalNoiseVariance = math.max(noiseVariance, PancakeDLRGP.numericalNoiseVariance)

  /**
    * Noise distribution, expands the low rank subspace distribution to whole domain/space
    */
  val noiseDistribution = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) * totalNoiseVariance)

  /** See [[DiscreteLowRankGaussianProcess.rank]] */
  val rank: Int = gpModel.rank

  // model matrices as needed by coefficients
  private val D: DenseVector[Double] = gpModel.variance
  private val S: DenseVector[Double] = breeze.numerics.sqrt(D)
  private val U: DenseMatrix[Double] = gpModel.basisMatrix
  private lazy val W: DenseMatrix[Double] = U * breeze.linalg.diag(S)

  // we cannot assume an orthonormal basis in general
  // e.g. Nystroem bases are not orthonormal w.r.t the matrix
  private lazy val Minv: DenseMatrix[Double] = {
    breeze.linalg.inv(
      ((U.t * U) * diag(S :* S)) + diag(DenseVector.ones[Double](rank) :* totalNoiseVariance)
    )
  }

  private val totalDim: Int = outputDim * domain.numberOfPoints

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
  def instance(c: DenseVector[Double]): DiscreteField[D, Value] = gpModel.instance(c)

  /**
    * Get instance at a specific point
    */
  def instanceAtPoint(c: DenseVector[Double], pid: PointId): Value = gpModel.instanceAtPoint(c, pid)

  /**
    * Returns the probability density of the instance produced by the x coefficients
    */
  def pdf(coefficients: DenseVector[Double]): Double = gpModel.pdf(coefficients)

  /**
    * Returns the log of the probability density of the instance produced by the x coefficients.
    *
    * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
    */
  def logpdf(coefficients: DenseVector[Double]): Double = gpModel.logpdf(coefficients)

  /**
    * Returns the probability density of the given instance, takes spherical noise term into account
    */
  override def pdf(instance: DiscreteField[D, Value]): Double = pdf(coefficients(instance))

  /**
    * Returns the log of the probability density of the instance
    *
    * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable,
    * takes spherical noise term into account
    */
  override def logpdf(instance: DiscreteField[D, Value]): Double = logpdf(coefficients(instance))

  /**
    * Draw a sample from the model, includes noise(!) (use underlying gpModel without noise)
    */
  override def sample()(implicit rnd: Random): DiscreteField[D, Value] = {
    val coeffs: DenseVector[Double] = coefficientsDistribution.sample()
    val componentNoise = Gaussian(0.0, totalNoiseVariance)
    val instVal = gpModel.instanceVector(coeffs).map{v => v + componentNoise.sample()}
    DiscreteField.createFromDenseVector[D, Value](gpModel.domain, instVal)
  }

  /**
    * Get the probability distribution of the coefficients in the latent space of the model (multivariate standard normal).
    *
    * @return MultivariateNormalDistribution(0,I).
    */
  lazy val coefficientsDistribution: MultivariateNormalDistribution = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), DenseMatrix.eye[Double](rank))

  /**
    * Project the sample into this model, best reconstruction
    */
  override def project(s: DiscreteField[D, Value]): DiscreteField[D, Value] = instance(coefficients(s))

  /**
    * Get the low-rank expansion coefficients of a sample, respects model noise
    */
  def coefficients(s: DiscreteField[D, Value]): DenseVector[Double] = {
    val instanceVector = DiscreteField.vectorize(s)
    assert(instanceVector.length == totalDim)
    Minv * (diag(S) * (U.t * (instanceVector - meanVector)))
  }

  /**
    * Calculate the posterior model given point observations with a common independent isotropic Gaussian noise
    *
    * @param sigma2 observation noise of sample, *additional* (independent) to model noise
    */
  def posterior(trainingData: IndexedSeq[(PointId, Value)], sigma2: Double): PancakeDLRGP[D, Value] = {
    require(sigma2 >= 0.0)
    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) :* (sigma2 + totalNoiseVariance))
    val newtd = trainingData.map { case (ptId, df) => (ptId, df, cov) }
    posterior(newtd)
  }

  /**
    * Calculate the posterior model given point observations with an individual uncertainty for each
    *
    * @param trainingData list of point observations (PointId, Value, Uncertainty), uncertainty is *additional* (independent) to model noise
    */
  def posterior(trainingData: IndexedSeq[(PointId, Value, MultivariateNormalDistribution)]): PancakeDLRGP[D, Value] = {
    def addMVN(mvn1: MultivariateNormalDistribution, mvn2: MultivariateNormalDistribution): MultivariateNormalDistribution = {
      MultivariateNormalDistribution(mvn1.mean + mvn2.mean, mvn1.cov + mvn2.cov)
    }
    val newTD = trainingData.map{ case (ptId, value, uncertainty) => (ptId, value, addMVN(noiseDistribution, uncertainty))}
    PancakeDLRGP(DiscreteLowRankGaussianProcess.regression(gpModel, trainingData), noiseVariance)
  }

  /**
    * Calculate the marginal distribution on a set of points
    * */
  override def marginal(pointIds: Seq[PointId])(implicit domainCreator: UnstructuredPointsDomain.Create[D]): PancakeDLRGP[D, Value] = {
    val gpMarginal = gpModel.marginal(pointIds)
    PancakeDLRGP(gpMarginal, noiseVariance)
  }
}

object PancakeDLRGP {
  val numericalNoiseVariance = 1e-15
  def apply[D <: Dim: NDSpace, Value](gpModel: DiscreteLowRankGaussianProcess[D, Value])(implicit vec: Vectorizer[Value]) = new PancakeDLRGP(gpModel, numericalNoiseVariance)
}
