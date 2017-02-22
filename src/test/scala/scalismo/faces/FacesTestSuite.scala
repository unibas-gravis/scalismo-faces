/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces

import scalismo.faces.color.{RGB, RGBA, SRGB}
import scalismo.faces.image.PixelImage
import org.scalatest._
import scalismo.geometry.Vector3D
import scalismo.utils.Random

class FacesTestSuite extends FunSpec with Matchers {

  implicit val rnd = Random(43)

  def randomRGB = RGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomSRGB: SRGB = SRGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomRGBA = RGBA(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomVector3D = Vector3D(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  def randomDouble: Double = rnd.scalaRandom.nextDouble()

  def randomImage(w: Int, h: Int): PixelImage[RGB] = PixelImage(w, h, (x, y) => randomRGB)
}
