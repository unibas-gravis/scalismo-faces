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

import scalismo.faces.image.PixelImage
import scalismo.faces.parameters.RenderParameter
import scalismo.sampling.{ProposalGenerator, UnitTransition}
import scalismo.utils.Random

/**
  * Combined Proposal for Segmentation and Parameters
  * Proposes either new mask or new Parameterset
  */
class SegmentationMasterProposal(paramProposal: ProposalGenerator[RenderParameter], maskProposal: ProposalGenerator[(RenderParameter,PixelImage[Int])], paramProposalProb: Double)
                                (implicit random: Random)
  extends ProposalGenerator[(RenderParameter, PixelImage[Int])]
    with UnitTransition[(RenderParameter, PixelImage[Int])]{

  private var gate = false
  override def propose(current: (RenderParameter, PixelImage[Int])): (RenderParameter, PixelImage[Int]) = {
    gate = random.scalaRandom.nextDouble() < paramProposalProb
    if (gate)
      (paramProposal.propose(current._1), current._2)
    else
      maskProposal.propose(current)
  }

  override def toString: String = {
    var str = s"MasterProposal("
    if (gate)
      str += paramProposal.toString + ")"
    else
      str += maskProposal.toString + ")"
    str
  }
}

object SegmentationMasterProposal {
  def apply(paramProposal: ProposalGenerator[RenderParameter],
            maskProposal:  ProposalGenerator[(RenderParameter,PixelImage[Int])],
            paramProposalProb: Double)
           (implicit random: Random) = new SegmentationMasterProposal(paramProposal, maskProposal, paramProposalProb)
}


