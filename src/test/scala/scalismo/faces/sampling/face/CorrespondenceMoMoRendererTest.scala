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

package scalismo.faces.sampling.face

import java.net.URI

import scalismo.faces.FacesTestSuite
import scalismo.faces.color.RGBA
import scalismo.faces.parameters.{MoMoInstance, Pose, RenderParameter}
import scalismo.faces.utils.LanguageUtilities
import scalismo.geometry.Vector

class CorrespondenceMoMoRendererTest extends FacesTestSuite {
  describe("A CorrespondenceMoMoRenderer") {
    lazy val randomMomo = randomGridModel(10, 5, 0.1, 5, 5, orthogonalExpressions = false)
    val moMoRenderer = MoMoRenderer(randomMomo).cached(0)
    val corrMoMoRenderer = CorrespondenceMoMoRenderer(randomMomo).cached(0)
    val param = RenderParameter.defaultSquare.
      withMoMo(MoMoInstance.zero(randomMomo, new URI("randomModel"))).
      withPose(Pose(1.0, Vector(0,0,-10), 0,0,0))

    def diffRGBA(a: RGBA, b: RGBA) = math.abs(a.r - b.r) + math.abs(a.g - b.g) + math.abs(a.b - b.b) + math.abs(a.a - b.a)

    it("renders the same as a MoMoRenderer") {
      val A = moMoRenderer.renderImage(param)
      val B = corrMoMoRenderer.renderImage(param)
      val sum = A.zip(B).map{case(a,b)=>diffRGBA(a,b)}.values.toIndexedSeq.sum
      sum should be <= 0.0
    }
  }
}
