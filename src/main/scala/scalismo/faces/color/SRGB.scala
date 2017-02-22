package scalismo.faces.color

import java.awt.Color
import scalismo.geometry.{Vector, _3D}

case class SRGB(r: Double, g: Double, b: Double) {
  require(r>=0.0 && g>= 0.0 && b >= 0.0, "sRGB color values must be >= 0.") // because what sould be done with gamma on negative values?

  /** converts to the linear sRGB values that can be used with linear operations. */
  def toRGB = GammaCorrection.toLinear(this)

  /** applies f to all channels */
  def map(f: Double => Double): SRGB = SRGB(f(r), f(g), f(b))

  def isInBounds: Boolean = r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0

  def clamped: SRGB = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      SRGB(clamp(r), clamp(g), clamp(b))
    else
      this
  }

  /** convert to AWT default color. expects a clamped color value */
  def toAWTColor: Color = new Color(SRGB.toInt8(r), SRGB.toInt8(g), SRGB.toInt8(b))
}

object SRGB {
  val White: SRGB = SRGB(1.0, 1.0, 1.0)
  val Black: SRGB = SRGB(0.0, 0.0, 0.0)

  def apply(color: RGBA): SRGB = new SRGB(color.r, color.g, color.b)
  def apply(gray: Double): SRGB = new SRGB(gray, gray, gray)
  def apply(tuple: (Double, Double, Double)) = new SRGB(tuple._1, tuple._2, tuple._3)
  def apply(vector3D: Vector[_3D]) = new SRGB(vector3D.x, vector3D.y, vector3D.z)
  def apply(awtColor: Color) = new SRGB(fromInt8(awtColor.getRed), fromInt8(awtColor.getGreen), fromInt8(awtColor.getBlue))

  private def toInt8(value: Double): Int = (value * 255.0).toInt
  private def fromInt8(intValue: Int): Double = intValue / 255.0
}

case class SRGBA(r: Double, g: Double, b: Double, a: Double) {
  require(r>=0.0 && g>= 0.0 && b >= 0.0, "sRGB color values must be >= 0.") // because what sould be done with gamma on negative values?

  def toRGBA = RGBA(this.toSRGB.toRGB, a)

  def toSRGB = SRGB(r, g, b)

  def isInBounds: Boolean = r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0 && a >= 0.0 && a <= 1.0

  /** clamp all values to valid range [0, 1] */
  def clamped: SRGBA = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      SRGBA(clamp(r), clamp(g), clamp(b), clamp(a))
    else
      this
  }

  /** convert to AWT default color. expects a clamped color value */
  def toAWTColor: Color = new Color(SRGBA.toInt8(r), SRGBA.toInt8(g), SRGBA.toInt8(b))
}

object SRGBA {
  val White: SRGBA = SRGBA(1.0, 1.0, 1.0, 1.0)
  val Black: SRGBA = SRGBA(0.0, 0.0, 0.0, 1.0)

  val WhiteTransparent: SRGBA = SRGBA(1.0, 1.0, 1.0, 0.0)
  val BlackTransparent: SRGBA = SRGBA(0.0, 0.0, 0.0, 0.0)

  def apply(color: SRGB): SRGBA = new SRGBA(color.r, color.g, color.b, 1.0)
  def apply(color: SRGB, a: Double): SRGBA = new SRGBA(color.r, color.g, color.b, a)
  def apply(r: Double, g: Double, b: Double): SRGBA = new SRGBA(r, g, b, 1.0)
  def apply(gray: Double): SRGBA = new SRGBA(gray, gray, gray, 1.0)
  def apply(gray: Double, a: Double): SRGBA = new SRGBA(gray, gray, gray, a)
  def apply(tuple: (Double, Double, Double, Double)): SRGBA = new SRGBA(tuple._1, tuple._2, tuple._3, tuple._4)
  def apply(awtColor: Color): SRGBA = SRGBA(fromInt8(awtColor.getRed), fromInt8(awtColor.getGreen), fromInt8(awtColor.getBlue), fromInt8(awtColor.getAlpha))

  private def toInt8(value: Double): Int = (value * 255.0).toInt

  private def fromInt8(intValue: Int): Double = intValue / 255.0

}


/**
  * This contains the gamma correction.
  * Naming convention:
  *   SRGB: Gamma distorted sRGB values
  *   RGB: linear sRGB values.
  *
  * Source: https://www.w3.org/Graphics/Color/srgb
  *
  * This implementation is consistent in round trip sRGB -> RGB and back (errors below 1e-16).
  * The java internal implementation is much less consistent (errors up to 0.06!)
  */
object GammaCorrection {

  /** sRGB to linear*/
  def toLinear(srgb: SRGB): RGB = {
    def transformChannel(c: Double) = {
      if(c <= 0.04045){
        c/12.92
      }else{
        math.pow((c+0.055)/1.055, 2.4)
      }
    }
    RGB(
      transformChannel(srgb.r),
      transformChannel(srgb.g),
      transformChannel(srgb.b)
    )
  }

  /** linear to sRGB */
  def toGamma(rgbLinear: RGB): SRGB = {
    def transformChannel(c: Double) = {
      if( c <= 0.0031308) {
        12.92*c
      }else{
        1.055 * math.pow(c, 1.0/2.4) - 0.055
      }
    }
    SRGB(
      transformChannel(rgbLinear.r),
      transformChannel(rgbLinear.g),
      transformChannel(rgbLinear.b)
    )
  }

}