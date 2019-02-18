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

package scalismo.faces.warp

import scalismo.faces.image.PixelImage
import scalismo.geometry.{EuclideanVector, _2D}

import scala.annotation.tailrec

/** invert warp field */
object WarpFieldInversion {
  /** invert a warp field, uses a fixed point iteration invW(x) = -W(x + invW(x)) */
  def fixedPointInversion(warpField: PixelImage[EuclideanVector[_2D]], iterations: Int = 5): PixelImage[EuclideanVector[_2D]] = {
    val contField = warpField.interpolate
    // iteration: iw(x) = -w( x + iw(x) )
    @tailrec
    def fixedPointIteration(invW: PixelImage[EuclideanVector[_2D]], iterations: Int): PixelImage[EuclideanVector[_2D]] = {
      if (iterations > 0) {
        // for each position pull back the correct warp field value
        val invField = PixelImage(warpField.domain, (x, y) => {
          val v = EuclideanVector(x, y) + invW(x, y)
          -contField(v.x, v.y): EuclideanVector[_2D]
        })
        fixedPointIteration(invField, iterations - 1)
      } else
        invW
    }
    // initial: iw(x) = 0
    val init: PixelImage[EuclideanVector[_2D]] = warpField.map(v => EuclideanVector(0f, 0f))
    // iterate: fixed-point iterations
    fixedPointIteration(init, iterations)
  }
}
