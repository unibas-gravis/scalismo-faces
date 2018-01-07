package faces.sampling.face.proposals

import scalismo.faces.image.PixelImage
import scalismo.faces.parameters.RenderParameter
import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio}
import scalismo.utils.Random

/**
  * Combined Proposal for Segmentation and Parameters
  * Proposes either new mask or new Parameterset
  */
class SegmentationMasterProposal(paramProposal: ProposalGenerator[RenderParameter], maskProposal: ProposalGenerator[(RenderParameter,PixelImage[Int])], paramProposalProb: Double)
                                (implicit random: Random)
  extends ProposalGenerator[(RenderParameter, PixelImage[Int])]
    with SymmetricTransitionRatio[(RenderParameter, PixelImage[Int])]{

  private var gate = false
  override def propose(current: (RenderParameter, PixelImage[Int])): (RenderParameter, PixelImage[Int]) = {
    gate = random.scalaRandom.nextDouble() < paramProposalProb
    if (gate)
      (paramProposal.propose(current._1), current._2)
    else
      maskProposal.propose(current)
  }

  override def toString= {
    var str = s"SegmentationMasterProposal("
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

