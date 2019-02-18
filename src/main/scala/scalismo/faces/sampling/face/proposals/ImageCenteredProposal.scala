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

import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.geometry.{Point, EuclideanVector}
import scalismo.sampling.{ProposalGenerator, TransitionProbability}

/**
  * proposal which keeps the image location of a selected landmark at a constant position, uses shifts of the principle point to keep the position fixed.
  * Compensates translation of the wrapped proposal
  *
  * @param proposal wrapped proposal, translation introduced by it will be compensated
  * @param lmId id of landmark to keep fixed (must be known by renderer)
  * @param lmRenderer parametric landmarks renderer to find image position of landmark
  */
class ImageCenteredProposal(val proposal: ProposalGenerator[RenderParameter] with TransitionProbability[RenderParameter],
                            val lmId: String,
                            val lmRenderer: ParametricLandmarksRenderer)
  extends ProposalGenerator[RenderParameter] with TransitionProbability[RenderParameter] {

  require(lmRenderer.hasLandmarkId(lmId), "landmark needs to be known for an ImageCenteredProposal")

  override def propose(current: RenderParameter): RenderParameter = {
    // render the landmark's image location before application of the proposal
    // keep track of the landmark position
    val lmCurrent: TLMSLandmark2D = lmRenderer.renderLandmark(lmId, current).get // get is safe, checked during construction

    // apply proposal
    val propSample = proposal.propose(current)

    // render the landmark after application
    val lmProposal: TLMSLandmark2D = lmRenderer.renderLandmark(lmId, propSample).get

    // shift back (compensate translation in image)
    val shift = lmCurrent.point - lmProposal.point

    // shifting by moving the principal point (normalized device coordinates [-1, 1], y axis upwards)
    val pp = Point(
      propSample.camera.principalPoint.x + 2 * shift.x / propSample.imageSize.width,
      propSample.camera.principalPoint.y - 2 * shift.y / propSample.imageSize.height
    )

    propSample.withCamera(propSample.camera.copy(principalPoint = pp))
  }

  /** rate of transition from to */
  override def logTransitionProbability(from: RenderParameter, to: RenderParameter): Double = {
    // return -inf if the shift is not compatible with this proposal
    // expected shift between from and to
    val lmFrom = lmRenderer.renderLandmark(lmId, from).get
    val lmTo = lmRenderer.renderLandmark(lmId, to).get
    val expectedShift = EuclideanVector(
      2 * (lmTo.point.x - lmFrom.point.x) / to.imageSize.width,
      2 * (lmTo.point.y - lmFrom.point.y) / to.imageSize.height
    )

    // actual shift
    val actualShift = EuclideanVector(
      to.camera.principalPoint.x - from.camera.principalPoint.x,
      -(to.camera.principalPoint.y - from.camera.principalPoint.y)
    )
    // difference
    val diff = actualShift - expectedShift
    if (diff.norm2 < 1e-4) {
      val virtualTo = to.copy(camera = to.camera.copy(principalPoint = from.camera.principalPoint))
      proposal.logTransitionProbability(from, virtualTo)
    } else // not compatible with the calculated shift in the image - no transition probability
      Double.NegativeInfinity
  }

  override def toString: String = s"ImageCenteredProposal($lmId, $proposal)"
}

object ImageCenteredProposal {
  /** construct an ImageCenteredProposal, fails for unknown landmarks */
  def apply(proposal: ProposalGenerator[RenderParameter] with TransitionProbability[RenderParameter],
            lmId: String,
            lmRenderer: ParametricLandmarksRenderer): Option[ImageCenteredProposal] = {
    if (lmRenderer.hasLandmarkId(lmId))
      Some(new ImageCenteredProposal(proposal, lmId, lmRenderer))
    else
      None
  }

  /** implicit builder for ImageCenteredProposal, attached to other proposal with .centeredAt(lm) */
  object implicits {
    implicit class RichProposal(proposal: ProposalGenerator[RenderParameter] with TransitionProbability[RenderParameter]) {
      /** create an ImageCenteredProposal wrapped around this proposal, centered at lmId */
      def centeredAt(lmId: String, lmRenderer: ParametricLandmarksRenderer): Option[ImageCenteredProposal] = {
        ImageCenteredProposal(proposal, lmId, lmRenderer)
      }
    }
  }
}