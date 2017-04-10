package scalismo.faces.color


/**
  * This contains the gamma correction.
  * Naming convention:
  *   sRGB: Gamma distorted values
  *   RGB: linear values.
  *
  * Source: https://www.w3.org/Graphics/Color/srgb
  *
  * This implementation is consistent in round trip sRGB -> RGB and back (errors below 1e-16).
  * The java internal implementation is much less consistent (errors up to 0.06!)
  */
object GammaCorrection {

  /** sRGB to linear*/
  def toLinear(srgb: RGB): RGB = {
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
  def toGamma(rgbLinear: RGB): RGB = {
    def transformChannel(c: Double) = {
      if( c <= 0.0031308) {
        12.92*c
      }else{
        1.055 * math.pow(c, 1.0/2.4) - 0.055
      }
    }
    RGB(
      transformChannel(rgbLinear.r),
      transformChannel(rgbLinear.g),
      transformChannel(rgbLinear.b)
    )
  }

}