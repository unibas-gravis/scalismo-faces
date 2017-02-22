package scalismo.faces.color

import java.awt.Color
import scalismo.geometry.{Vector, _3D}

case class sRGB(r: Double, g: Double, b: Double) {
  require(r>=0.0 && g>= 0.0 && b >= 0.0, "sRGB color values must be >= 0.") // because what sould be done with gamma on negative values?

  /** converts to the linear sRGB values that can be used with linear operations. */
  def toRGB = GammaCorrection.toLinear(this)

  /** applies f to all channels */
  def map(f: Double => Double): sRGB = sRGB(f(r), f(g), f(b))

  def isInBounds: Boolean = r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0

  def clamped: sRGB = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      sRGB(clamp(r), clamp(g), clamp(b))
    else
      this
  }

  /** convert to AWT default color. expects a clamped color value */
  def toAWTColor: Color = new Color(sRGB.toInt8(r), sRGB.toInt8(g), sRGB.toInt8(b))
}

object sRGB {
  val White: sRGB = sRGB(1.0, 1.0, 1.0)
  val Black: sRGB = sRGB(0.0, 0.0, 0.0)

  def apply(color: RGBA): sRGB = new sRGB(color.r, color.g, color.b)
  def apply(gray: Double): sRGB = new sRGB(gray, gray, gray)
  def apply(tuple: (Double, Double, Double)) = new sRGB(tuple._1, tuple._2, tuple._3)
  def apply(vector3D: Vector[_3D]) = new sRGB(vector3D.x, vector3D.y, vector3D.z)
  def apply(awtColor: Color) = new sRGB(fromInt8(awtColor.getRed), fromInt8(awtColor.getGreen), fromInt8(awtColor.getBlue))

  private def toInt8(value: Double): Int = (value * 255.0).toInt
  private def fromInt8(intValue: Int): Double = intValue / 255.0
}

case class sRGBA(r: Double, g: Double, b: Double, a: Double) {
  require(r>=0.0 && g>= 0.0 && b >= 0.0, "sRGB color values must be >= 0.") // because what sould be done with gamma on negative values?

  def toRGBA = RGBA(this.tosRGB.toRGB, a)

  def tosRGB = sRGB(r, g, b)

  def isInBounds: Boolean = r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0 && a >= 0.0 && a <= 1.0

  /** clamp all values to valid range [0, 1] */
  def clamped: sRGBA = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      sRGBA(clamp(r), clamp(g), clamp(b), clamp(a))
    else
      this
  }

  /** convert to AWT default color. expects a clamped color value */
  def toAWTColor: Color = new Color(sRGBA.toInt8(r), sRGBA.toInt8(g), sRGBA.toInt8(b))
}

object sRGBA {
  val White: sRGBA = sRGBA(1.0, 1.0, 1.0, 1.0)
  val Black: sRGBA = sRGBA(0.0, 0.0, 0.0, 1.0)

  val WhiteTransparent: sRGBA = sRGBA(1.0, 1.0, 1.0, 0.0)
  val BlackTransparent: sRGBA = sRGBA(0.0, 0.0, 0.0, 0.0)

  def apply(color: sRGB): sRGBA = new sRGBA(color.r, color.g, color.b, 1.0)
  def apply(color: sRGB, a: Double): sRGBA = new sRGBA(color.r, color.g, color.b, a)
  def apply(r: Double, g: Double, b: Double): sRGBA = new sRGBA(r, g, b, 1.0)
  def apply(gray: Double): sRGBA = new sRGBA(gray, gray, gray, 1.0)
  def apply(gray: Double, a: Double): sRGBA = new sRGBA(gray, gray, gray, a)
  def apply(tuple: (Double, Double, Double, Double)): sRGBA = new sRGBA(tuple._1, tuple._2, tuple._3, tuple._4)
  def apply(awtColor: Color): sRGBA = sRGBA(fromInt8(awtColor.getRed), fromInt8(awtColor.getGreen), fromInt8(awtColor.getBlue), fromInt8(awtColor.getAlpha))

  private def toInt8(value: Double): Int = (value * 255.0).toInt

  private def fromInt8(intValue: Int): Double = intValue / 255.0

}


/**
  * This contains the gamma correction.
  * Naming convention:
  *   sRGB: Gamma distorted sRGB values
  *   RGB: linear sRGB values.
  *
  * Source: https://www.w3.org/Graphics/Color/srgb
  *
  * This implementation is consistent in round trip sRGB -> RGB and back (errors below 1e-16).
  * The java internal implementation is much less consistent (errors up to 0.06!)
  */
object GammaCorrection {

  /** sRGB to linear*/
  def toLinear(srgb: sRGB): RGB = {
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
  def toGamma(rgbLinear: RGB): sRGB = {
    def transformChannel(c: Double) = {
      if( c <= 0.0031308) {
        12.92*c
      }else{
        1.055 * math.pow(c, 1.0/2.4) - 0.055
      }
    }
    sRGB(
      transformChannel(rgbLinear.r),
      transformChannel(rgbLinear.g),
      transformChannel(rgbLinear.b)
    )
  }

}