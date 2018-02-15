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

package scalismo.faces.sampling.face.proposals

import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.deluminate.SphericalHarmonicsOptimizer
import scalismo.faces.image.{AccessMode, PixelImage}
import scalismo.faces.mesh.{MeshSurfaceSampling, VertexColorMesh3D}
import scalismo.faces.parameters.{RenderParameter, SphericalHarmonicsLight}
import scalismo.faces.sampling.evaluators.LogNormalDistribution
import scalismo.faces.sampling.face.evaluators.PixelEvaluators.IsotropicGaussianPixelEvaluatorHSV
import scalismo.faces.sampling.face.{ParametricImageRenderer, ParametricModel}
import scalismo.faces.texture.TextureExtraction
import scalismo.geometry.{Vector, _3D}
import scalismo.mesh._
import scalismo.sampling.evaluators.{GaussianEvaluator, PairEvaluator}
import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio, TransitionProbability}
import scalismo.utils.Random

import scala.math._

/** illumination proposals */
object SphericalHarmonicsLightProposals {

  /** vary the light intensity
    * @param logSdev log of expected variation factor
    */
  case class SHLightIntensityProposal(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[SphericalHarmonicsLight] with TransitionProbability[SphericalHarmonicsLight] {

    override def propose(current: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      val factor = exp(rnd.scalaRandom.nextGaussian() * logSdev * 0.5)
      current.copy(coefficients = current.coefficients.map(_ * factor: Vector[_3D]))
    }

    override def logTransitionProbability(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): Double = {
      LogNormalDistribution.logDensity(math.sqrt(to.energy/from.energy), 0.0, logSdev)
    }
  }

  /** vary the color distribution of the illumination while keeping the intensity constant */
  case class SHLightColorProposal(sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[SphericalHarmonicsLight] with SymmetricTransitionRatio[SphericalHarmonicsLight] with TransitionProbability[SphericalHarmonicsLight] {

    override def propose(current: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      val shift = Vector(
        rnd.scalaRandom.nextGaussian() * sdev,
        rnd.scalaRandom.nextGaussian() * sdev,
        rnd.scalaRandom.nextGaussian() * sdev
      )
      val proposal = current.copy(coefficients = current.coefficients.map(v => v + shift))
      val intensityChange = sqrt(proposal.energy / current.energy)
      proposal.copy(coefficients = proposal.coefficients.map(v => v / intensityChange: Vector[_3D]))
    }

    override def logTransitionProbability(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): Double = {
      require(from.coefficients.length == to.coefficients.length, "the same number of SH components")
      val shifts =to.coefficients.zip(from.coefficients).map{case(t, f) => t - f}
      val allShiftsEqual = shifts.forall(shift => (shift - shifts.head).norm2 < 1e-5)
      if (math.abs(from.energy - to.energy) < 1e-5 && allShiftsEqual) {
        val (t, f) = (to.coefficients.head, from.coefficients.head)
        GaussianEvaluator.logDensity(t.x, f.x, sdev) +
          GaussianEvaluator.logDensity(t.y, f.y, sdev) +
          GaussianEvaluator.logDensity(t.z, f.z, sdev)
      }
      else
        Double.NegativeInfinity
    }
  }

  /** independent Gaussian perturbation of the SH illumination parameter
    * @param sdev standard deviation of proposal per component
    * @param fixIntensity if true the intensity is perserved */
  case class SHLightPerturbationProposal(sdev: Double, fixIntensity: Boolean)(implicit rnd: Random)
    extends ProposalGenerator[SphericalHarmonicsLight] with
      SymmetricTransitionRatio[SphericalHarmonicsLight] with
      TransitionProbability[SphericalHarmonicsLight] {

    override def propose(current: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      val proposal = current.copy(coefficients = current.coefficients.map(v => v + Vector(
        rnd.scalaRandom.nextGaussian(),
        rnd.scalaRandom.nextGaussian(),
        rnd.scalaRandom.nextGaussian()
      ) * sdev ))
      val intensityChange = sqrt(proposal.energy / current.energy)
      if (fixIntensity)
        proposal.copy(coefficients = proposal.coefficients.map(v => v / intensityChange: Vector[_3D]))
      else
        proposal
    }

    override def logTransitionProbability(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): Double = {
      val intChange = math.sqrt(to.energy/from.energy)
      if (!fixIntensity || intChange < 1e-5)
        to.coefficients.zip(from.coefficients).map{case(t, f) =>
          GaussianEvaluator.logDensity(t.x, f.x, sdev) +
            GaussianEvaluator.logDensity(t.y, f.y, sdev) +
            GaussianEvaluator.logDensity(t.z, f.z, sdev)
        }.sum
      else
        Double.NegativeInfinity

    }
  }

  /**
    * Randomly changes the light intensity of the shl bands according to std of a normal distribution.
    *
    * @param logSdev log of expected factor of variation for each component
    */
  case class SHLightBandEnergyMixer(logSdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[SphericalHarmonicsLight] with TransitionProbability[SphericalHarmonicsLight] {

    override def propose(current: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      if (current.nonEmpty) {
        val bands = sqrt(current.coefficients.size).floor.toInt
        require(bands * bands - current.coefficients.size == 0, "SHLightBandEnergyMixer: number of bands unknown")
        val coeff: Array[Vector[_3D]] = current.coefficients.toArray
        for (b <- 1 to bands) {
          val factor = math.exp(rnd.scalaRandom.nextGaussian() * logSdev)
          for (i <- (b - 1) * (b - 1) until b * b) {
            coeff(i) = coeff(i) * factor
          }
        }
        SphericalHarmonicsLight(coeff.toIndexedSeq)
      } else {
        current
      }
    }

    override def logTransitionProbability(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): Double = {
      val toVec = to.coefficients
      val fromVec = from.coefficients
      if (toVec.size != fromVec.size)
        Double.NegativeInfinity
      else if (toVec.isEmpty) {
        0.0
      } else {
        var totProb = 0.0
        val bands = math.sqrt(toVec.size).toInt
        require(bands * bands - toVec.size == 0, "unknown number of bands")
        for (b <- 1 to bands) {
          var intFrom = 0.0
          var intTo = 0.0
          for (i <- (b - 1) * (b - 1) until b * b) {
            intFrom = intFrom + fromVec(i).dot(fromVec(i))
            intTo = intTo + toVec(i).dot(toVec(i))
          }
          if (intTo > 1e-4 && intFrom > 1e-4) {
            val logFactor = math.log(intTo / intFrom)
            totProb = totProb + -0.5 * math.pow(logFactor / logSdev, 2) - 0.5 * math.log(2.0 * math.Pi) - math.log(logSdev)
          }
        }
        totProb
      }
    }
  }

  /**
    * Draw from a proposalGenerator in a way such that it has the same illumination intensity as a given sample.
    * We adjust the intensity of a sample drawn from proposalGenerator, so that it has the same intensity as the given proposal.
    *
    * @param proposalGenerator underlying proposal generator to use, proposal of it will get a rescaled light
    */
  case class SHLightIntensityPreservingProposal(proposalGenerator: ProposalGenerator[SphericalHarmonicsLight] with TransitionProbability[SphericalHarmonicsLight])(implicit rnd: Random)
    extends ProposalGenerator[SphericalHarmonicsLight] with TransitionProbability[SphericalHarmonicsLight] {

    /**
      * Scale "to" according fromIntensity/toIntensity
      * Calculate intensity of to.
      * Calculate intensity of from.
      */
    def scaleSample(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      val toVec = to.coefficients
      val fromVec = from.coefficients
      val intTo = toVec.foldLeft(0.0)((acc, v) => acc + v.dot(v))
      val intFrom = fromVec.foldLeft(0.0)((acc, v) => acc + v.dot(v))
      val factor = math.sqrt(intFrom / intTo)
      val adjustedTo = SphericalHarmonicsLight(toVec.map(v => v * factor))
      adjustedTo
    }

    /**
      * Scale the new sample by the factor currentIntensity/newSampleIntensity.
      */
    override def propose(current: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      val newSample = proposalGenerator.propose(current)
      scaleSample(current, newSample)
    }

    /** rate of transition from to */
    override def logTransitionProbability(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): Double = {
      if (to.coefficients.size != from.coefficients.size)
        Double.NegativeInfinity
      else if (to.coefficients.isEmpty) {
        0.0
      } else {
        val adjustedTo = scaleSample(from, to)
        proposalGenerator.logTransitionProbability(from, adjustedTo)
      }
    }
  }

  /**
    * Perturbs the shl coefficients, without changing the color.
    */
  case class SHLightSpatialPerturbation(sdev: Double)(implicit rnd: Random)
    extends ProposalGenerator[SphericalHarmonicsLight] with TransitionProbability[SphericalHarmonicsLight] {
    val monochromaticityThreshold = 1e-3
    /**
      * Add random normal monochromatic values to current.
      */
    override def propose(current: SphericalHarmonicsLight): SphericalHarmonicsLight = {
      SphericalHarmonicsLight(current.coefficients.map(v => {
        val value = rnd.scalaRandom.nextGaussian() * sdev
        v + Vector(value, value, value) //monochrome: add the same to every channel
      }))
    }

    /**
      * Checks if the change between from and to was due to this proposal generator.
      * In the case of non-monochromatic changes we yield -inf.
      * In the case of monochromatic change we return the probability.
      * The probability depends only on the amount of the chromatic change.
      */
    override def logTransitionProbability(from: SphericalHarmonicsLight, to: SphericalHarmonicsLight): Double = {
      if (to.coefficients.size != from.coefficients.size)
        Double.NegativeInfinity
      else if (to.coefficients.isEmpty) {
        0.0
      } else {
        val toVec = to.coefficients
        val fromVec = from.coefficients
        var totProb = 0.0
        for (i <- toVec.indices) {
          val diff = toVec(i) - fromVec(i)
          if (math.abs(diff.x - diff.y) + math.abs(diff.y - diff.z) < monochromaticityThreshold) { //if monochromatic change, then we could take any channel, we choose the first one.
            totProb += 3.0 * (-0.5 * math.pow(diff.x / sdev, 2) - 0.5 * math.log(2.0 * math.Pi) - math.log(sdev))
          } else {
            return Double.NegativeInfinity //if non monochromatic change
          }
        }
        totProb
      }
    }
  }

  /** use the linear solver to find the best illumination parameters */
  case class SHLightSolverProposal(sphericalHarmonicsOptimizer: SphericalHarmonicsOptimizer,
                                   samplingFunction: TriangleMesh3D => IndexedSeq[(TriangleId, BarycentricCoordinates)])(implicit rnd: Random)
    extends ProposalGenerator[RenderParameter] with TransitionProbability[RenderParameter] {
    override def propose(current: RenderParameter): RenderParameter = current.copy(
      environmentMap = sphericalHarmonicsOptimizer.optimize(current, samplingFunction)
    )

    override def logTransitionProbability(from: RenderParameter, to: RenderParameter): Double = 0.0

    override def toString = s"SHLightSolverProposal($sphericalHarmonicsOptimizer)"
  }

  /** This proposal performs RANSAC to try to estimate illumination excluding outliers/occlusions
    * The face model is used as appearance prior
    *
    * The procedure is explained in:
    * Occlusion-aware 3D Morphable Face Models,
    * Bernhard Egger, Andreas Schneider, Clemens Blumer, Andreas Morel-Forster, Sandro Schönborn, Thomas Vetter
    * IN: British Machine Vision Conference (BMVC), September 2016
    * https://dx.doi.org/10.5244/C.30.64
    * */
  case class  RobustSHLightSolverProposal(modelRenderer: ParametricImageRenderer[RGBA] with ParametricModel,
                                          shOpt: SphericalHarmonicsOptimizer,
                                          target: PixelImage[RGBA],
                                          pixelEvaluator: PairEvaluator[RGB] = IsotropicGaussianPixelEvaluatorHSV(0.043f),
                                          nSamples: Int = 30,
                                          sigmaThreshold: Double = 2,
                                          percentage: Double = 0.4,
                                          iterations: Int = 500,
                                          nSamplesIllumination: Int = 1000)(implicit rnd: Random)
    extends ProposalGenerator[RenderParameter] with TransitionProbability[RenderParameter] {

    val proposal = RobustSHLightSolverProposalWithLabel(modelRenderer,
      shOpt,
      target,
      pixelEvaluator,
      nSamples,
      sigmaThreshold,
      percentage,
      iterations,
      nSamplesIllumination)

    val dummyImg = target.map(_ => 0)

    // wrapping proposal with label
    override def propose(current: RenderParameter): RenderParameter = {
      proposal.propose(current,dummyImg)._1
    }

    override def logTransitionProbability(from: RenderParameter, to: RenderParameter): Double = 0.0

    override def toString = s"" // keep empty since it is only wrapping RobustSHLightSolverProposalWithLabel
  }

  /** This proposal performs RANSAC to try to estimate illumination excluding outliers/occlusions
    * The face model is used as appearance prior
    *
    * The procedure is explained in:
    * Occlusion-aware 3D Morphable Face Models,
    * Bernhard Egger, Andreas Schneider, Clemens Blumer, Andreas Morel-Forster, Sandro Schönborn, Thomas Vetter
    * IN: British Machine Vision Conference (BMVC), September 2016
    * https://dx.doi.org/10.5244/C.30.64
    * */
  case class  RobustSHLightSolverProposalWithLabel(modelRenderer: ParametricImageRenderer[RGBA] with ParametricModel,
                                                   shOpt: SphericalHarmonicsOptimizer,
                                                   target: PixelImage[RGBA],
                                                   pixelEvaluator: PairEvaluator[RGB] = IsotropicGaussianPixelEvaluatorHSV(0.043f),
                                                   nSamples: Int = 30,
                                                   sigmaThreshold: Double = 2,
                                                   percentage: Double = 0.4,
                                                   iterations: Int = 500,
                                                   nSamplesIllumination: Int = 1000)(implicit rnd: Random)
    extends ProposalGenerator[(RenderParameter, PixelImage[Int])] with TransitionProbability[(RenderParameter, PixelImage[Int])] {

    val normalizer = pixelEvaluator.logValue(RGB.White, RGB.White)
    val threshold: Double = Math.exp(-0.5 * Math.pow(sigmaThreshold, 2) - normalizer)

    /** this function optimizes the illumination parameters for a given set of points
      * and measures the quality of those parameters.
      * @param points a set of points the illumination should be optimized to
      * @param rps the current renderparameters (illumination part is ignored)
      * @return a copy of the input render parameters with the new estimated illumination
      *         the quality of this estimation
      *         an image indicating which parts of the image are consistent to this estimated illumination
      * */
    private def estimateAndEvaluate(points: IndexedSeq[(TriangleId, BarycentricCoordinates)], rps: RenderParameter): (RenderParameter, Double, PixelImage[Int]) = {
      val estimatedLight = shOpt.optimize(rps, points)
      val estimatedRps: RenderParameter = rps.copy(environmentMap = estimatedLight)

      val curSample: PixelImage[RGBA] = modelRenderer.renderImage(estimatedRps).withAccessMode(AccessMode.Strict[RGBA])
      var counter = 0

      //calculating difference per pixel
      val diff = (x: Int, y: Int) => {
        val c = curSample(x,y)
        if (c.a > 0  ) {
          counter = counter + 1
          pixelEvaluator.logValue(c.toRGB, target(x, y).toRGB)
        }
        else
        // handling of pixels to in the face region to be above threshold
          Math.log(threshold) - normalizer + 1
      }

      val differenceImage = PixelImage(curSample.domain, diff, AccessMode.Strict[Double])
      val thresholded = differenceImage.map(d => if (Math.exp(d - normalizer) > threshold) 1 else 0)
      val count: Double = thresholded.values.sum.toDouble / counter
      (estimatedRps, count, thresholded)
    }

    // samples according to mask from image
    private def maskedSampler(rps: RenderParameter, label: PixelImage[Int]) = {
      val maskProp = labelAsSurfaceProperty(rps, label.withAccessMode(AccessMode.Strict[Int]))
      MeshSurfaceSampling.sampleAccordingToMask(maskProp.map(v => v.getOrElse(0.0)), nSamplesIllumination) _
    }

    // puts the label from the image to the mesh as surface property
    private def labelAsSurfaceProperty(rps: RenderParameter, label: PixelImage[Int]): MeshSurfaceProperty[Option[Double]] = {
      TextureExtraction.imageAsSurfaceProperty(modelRenderer.instance(rps).shape, rps.pointShader, label.map(i => i.toDouble))
    }


    override def propose(current: (RenderParameter, PixelImage[Int])): (RenderParameter, PixelImage[Int]) = {

      // get renderParameters from tuple, the labels from PixelImage[Int] are not used
      val rps = current._1

      val mesh: VertexColorMesh3D = modelRenderer.instance(rps)
      val faceMask = modelRenderer.renderImage(rps)
      val faceMaskLabel: PixelImage[Int] = faceMask.map(p => if (p.a > 0.01) {
        1
      } else {
        0
      })

      // RANSAC is an iterative method, those variables are keeping track of the current best estimate
      // the naming of the parameters is motivated by https://en.wikipedia.org/wiki/Random_sample_consensus
      var soFarBestModelQuality = 0.0
      var soFarBestLight: RenderParameter = rps
      var label = faceMaskLabel

      // while iterations < k {
      for (counter <- 1 until iterations) {
        // the sampler to sample points on the surface
        val points = MeshSurfaceSampling.sampleUniformlyOnSurface(nSamples)(mesh.shape)

        // maybemodel = model parameters fitted to maybeinliers
        val (light, count, thresholded) = estimateAndEvaluate(points, rps)

        // do a full model estimation if the estimation based on few samples is good enough
        if (count > percentage) {
          // the full model estimation takes 1000 instead of 30 samples (with standard parameters) but is restricted to estimated mask
          val allPoints = maskedSampler(rps, thresholded)(mesh.shape)
          val (betterLight, betterCount, betterThresholded) = estimateAndEvaluate(allPoints,rps)

          // check if new estimate is better than so far best one
          if (betterCount > soFarBestModelQuality) {
            soFarBestLight = betterLight
            soFarBestModelQuality = betterCount
            label = betterThresholded
          }
        }
      }

      // update environmentMap in parameters
      val parameters = rps.copy(environmentMap = {
        val points = maskedSampler(rps, label)(mesh.shape)
        shOpt.optimize(rps, points)
      }

      )
      (parameters, label)
    }


    override def logTransitionProbability(from: (RenderParameter, PixelImage[Int]), to: (RenderParameter, PixelImage[Int])): Double = 0.0

    override def toString = s"RobustSHLightSolverProposalWithLabel($shOpt)"
  }
}
