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

package scalismo.faces.gui

import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JSlider, SwingConstants}

/** slider which works on a continuous range, uses 100 ticks */
class MappedSlider(orientation: Int, min: Double, max: Double, value: Double, changeListener: Double => Unit)
    extends JSlider {

  addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = changeListener(sliderToRange(getValue))
  })

  /** map the continuous value to the slider range 0,1,...,100 */
  private def rangeToSlider(value: Double): Int = ((value - min) / (max - min) * 100).toInt

  /** map the slider value to the continuous range [min, max] */
  private def sliderToRange(value: Int): Double = value.toDouble / 100.0 * (max - min) + min

  /** update the slider value */
  def updateValue(value: Double): Unit = setValue(rangeToSlider(value))
}

object MappedSlider {

  /** create a MappedSlider */
  def apply(orientation: Int, min: Double, max: Double, value: Double, changeListener: Double => Unit) =
    new MappedSlider(orientation, min, max, value, changeListener)

  /** create a vertical MappedSlider */
  def apply(min: Double, max: Double, value: Double, changeListener: Double => Unit) =
    new MappedSlider(SwingConstants.VERTICAL, min, max, value, changeListener)

  /** create a MappedSlider */
  def apply(min: Double, max: Double, value: Double, changeListener: Double => Unit, orientation: Int) =
    new MappedSlider(orientation, min, max, value, changeListener)
}
