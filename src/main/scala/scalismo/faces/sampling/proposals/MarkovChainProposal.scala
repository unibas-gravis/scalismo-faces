/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.faces.sampling.proposals

import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio}
import scalismo.utils.Random



class MarkovChainProposal[A](val chain: Iterator[A], val numSamples : Int) (implicit rnd: Random)
  extends ProposalGenerator[A] with SymmetricTransitionRatio[A] {

  override def propose(current: A): A = {
    chain.drop(numSamples-1).next
  }

}

object MarkovChainProposal {
  def apply[A](chain: Iterator[A], numSamples: Int) = new MarkovChainProposal[A](chain,numSamples)
}