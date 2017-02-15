package scalismo.faces.color

case class sRGB(r: Double, g: Double, b: Double) {
  require(r>=0.0 && g>= 0.0 && b >= 0.0, "sRGB color values must be >= 0.") // because what sould be done with gamma on negative values?

  /** converts to the linear sRGB values that can be used with linear operations. */
  def toRGB = RGBsRGBConversion.toLinear(this)

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
}

object sRGB {
  val White: sRGB = sRGB(1.0, 1.0, 1.0)
  val Black: sRGB = sRGB(0.0, 0.0, 0.0)
  def apply(gray: Double): sRGB = new sRGB(gray, gray, gray)
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
}

object sRGBA {
  val White: sRGBA = sRGBA(1.0, 1.0, 1.0, 1.0)
  val Black: sRGBA = sRGBA(0.0, 0.0, 0.0, 1.0)

  val WhiteTransparent: sRGBA = sRGBA(1.0, 1.0, 1.0, 0.0)
  val BlackTransparent: sRGBA = sRGBA(0.0, 0.0, 0.0, 0.0)

  def apply(color: sRGB): sRGBA = new sRGBA(color.r, color.g, color.b, 1.0)
  def apply(color: sRGB, alpha: Double): sRGBA = new sRGBA(color.r, color.g, color.b, alpha)


}


/**
  * This contains the gamma correction.
  * Naming convention:
  *   sRGB: Gamma distorted sRGB values
  *   RGB: linear sRGB values.
  * RGB color values can be used during rendering.
  * Applying linear transforms to sRGB leads to wrong results.
  *
  * Source of transformations: https://en.wikipedia.org/wiki/SRGB#Specification_of_the_transformation
  */
object RGBsRGBConversion {

  /** to linear sRGB*/
  def toLinear(srgb: sRGB): RGB = {
    def transformChannel(c: Double) = {
      if(c <= 0.04045){
        c/12.92
      }else{
        math.pow((c+0.05)/1.05, 2.4)
      }
    }
    RGB(
      transformChannel(srgb.r),
      transformChannel(srgb.g),
      transformChannel(srgb.b)
    )
  }

  /** linear to sRGB */
  def tosRGB(rgbLinear: RGB): sRGB = {
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