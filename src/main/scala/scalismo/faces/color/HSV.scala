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

package scalismo.faces.color

/** HSV color value with Hue in [0.0,2*Pi), Saturation in [0.0,1.0] and Value in [0.0,1.0] */
case class HSV(hue: Double, saturation: Double, value: Double) {

  /** convert to RGB value. May throw or result in undefined behaviour if values outside of ranges.*/
  def toRGB: RGB = {
    val hs = hue / (math.Pi / 3.0)
    val h: Int = hs.toInt
    val f: Double = hs - h
    val p: Double = value * (1.0 - saturation)
    val q: Double = value * (1.0 - saturation * f)
    val t: Double = value * (1.0 - saturation * (1.0 - f))
    h match {
      case 0 => RGB(value, t, p)
      case 1 => RGB(q, value, p)
      case 2 => RGB(p, value, t)
      case 3 => RGB(p, q, value)
      case 4 => RGB(t, p, value)
      case 5 => RGB(value, p, q)
      case 6 => RGB(value, t, p)
      case _ => throw new RuntimeException(s"Invalid hue value (${h}) in conversion of color ${this}.")
    }
  }
}

object HSV {

  /** convert from RGB value. */
  def apply(rgb: RGB): HSV = {
    val maxCh: Double = math.max(rgb.r, math.max(rgb.g, rgb.b))
    val minCh: Double = math.min(rgb.r, math.min(rgb.g, rgb.b))
    val RGB(r, g, b) = rgb
    val h: Double = maxCh match {
      case `minCh` => 0.0
      case `r` => math.Pi / 3.0 * (0.0 + (g - b) / (maxCh - minCh))
      case `g` => math.Pi / 3.0 * (2.0 + (b - r) / (maxCh - minCh))
      case `b` => math.Pi / 3.0 * (4.0 + (r - g) / (maxCh - minCh))
    }
    val s = if (maxCh > 0.0) (maxCh - minCh) / maxCh else 0.0
    val v = maxCh
    HSV( if(h<0) h+2.0*math.Pi else h, s, v)
  }

  /** ColorBlender for HSV colors */
  implicit object HSVBlender extends ColorBlender[HSV] {
    /** Blend two colors (or other objects), necessary for interpolation in images, l is within [0, 1] */
    override def blend(obj1: HSV, obj2: HSV, l: Double): HSV = {
      val m = 1.0 - l
      val h1 = obj1.hue
      val h2 = obj2.hue
      HSV(
        math.atan2(l * math.sin(h1) + m * math.cos(h1), l * math.sin(h2) + m * math.cos(h2)),
        obj1.saturation * l + obj2.saturation * m,
        obj1.value * l + obj2.value * m
      )
    }
  }
}
