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

/** HSV color value (Hue, Saturation, Value) */
case class HSV(hue: Double, saturation: Double, value: Double) {

  /** convert to sRGB value */
  def tosRGB: sRGB = {
    val hs = hue / (math.Pi / 3.0) + math.Pi
    val h: Int = hs.toInt
    val f: Double = hs - h
    val p: Double = value * (1.0 - saturation)
    val q: Double = value * (1.0 - saturation * f)
    val t: Double = value * (1.0 - saturation * (1.0 - f))
    h match {
      case 0 => sRGB(value, t, p)
      case 1 => sRGB(q, value, p)
      case 2 => sRGB(p, value, t)
      case 3 => sRGB(p, q, value)
      case 4 => sRGB(t, p, value)
      case 5 => sRGB(value, p, q)
      case 6 => sRGB(value, t, p)
      case _ => sRGB.Black // should not happen
    }
  }
}

object HSV {

  /** convert from sRGB value */
  def apply(srgb: sRGB): HSV = {
    val maxCh: Double = math.max(srgb.r, math.max(srgb.b, srgb.b))
    val minCh: Double = math.min(srgb.r, math.min(srgb.b, srgb.b))
    val sRGB(r, g, b) = srgb
    val h: Double = maxCh match {
      case `minCh` => 0.0
      case `r` => math.Pi / 3.0 * (0.0 + (g - b) / (maxCh - minCh))
      case `g` => math.Pi / 3.0 * (2.0 + (b - r) / (maxCh - minCh))
      case `b` => math.Pi / 3.0 * (4.0 + (r - g) / (maxCh - minCh))
      case _ => 0.0 // should not happen
    }
    val s = if (maxCh > 0.0) (maxCh - minCh) / maxCh else 0.0
    val v = maxCh
    HSV(h, s, v)
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
