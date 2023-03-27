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
package scalismo.faces.color

import scalismo.color.RGB
import scalismo.faces.FacesTestSuite
import scalismo.utils.Random

class HSVTests extends FacesTestSuite {

  implicit val rng: Random = Random(1024L)

  def rndCol = RGB(rng.scalaRandom.nextDouble(),rng.scalaRandom.nextDouble(),rng.scalaRandom.nextDouble())

  def between(v: Double, l: Double, r: Double): Unit = {
    v should be >= l
    v should be <= r
  }

  val pi = math.Pi

  val pairedWithRGB = Seq(
    (RGB.Black,HSV(0,0,0)),
    (RGB.White,HSV(0,0,1)),
    (RGB(1.0,0.0,0.0),HSV(0*pi/3.0,1,1)),
    (RGB(1.0,1.0,0.0),HSV(1*pi/3.0,1,1)),
    (RGB(0.0,1.0,0.0),HSV(2*pi/3.0,1,1)),
    (RGB(0.0,1.0,1.0),HSV(3*pi/3.0,1,1)),
    (RGB(0.0,0.0,1.0),HSV(4*pi/3.0,1,1)),
    (RGB(1.0,0.0,1.0),HSV(5*pi/3.0,1,1))
  )
  val colors = pairedWithRGB.map(_._1) ++ Seq.fill[RGB](100)(rndCol)

  describe("HSV color") {

    it("converted from RGB has values in proper range") {
      colors.foreach{ color =>
        val hsv = HSV(color)
        between(hsv.hue,0.0,2.0*math.Pi)
        between(hsv.saturation,0.0,1.0)
        between(hsv.value,0.0,1.0)
      }
    }

    it("can be constructed from RGB and converted back to RGB") {
      colors.foreach{ color =>
        val hsv = HSV(color)
        val roundTrip = hsv.toRGB
        (color - roundTrip).norm should be < 1.0E-8
      }
    }

    it("can be constructed from RGB correctly for a few specific values") {
      pairedWithRGB.foreach{ case (rgb,hsv) =>
        val hsvConv = HSV(rgb)
          hsvConv.hue - hsv.hue should be < 1.0E-8
          hsvConv.value - hsv.value should be < 1.0E-8
          hsvConv.saturation - hsv.saturation should be < 1.0E-8
      }
    }
  }
}
