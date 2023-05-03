package scalismo.faces.io.renderparameters.fromats

import scalismo.color.RGB
import scalismo.faces.parameters.ColorTransform

case class ColorTransformLegacyFormat(gain: RGB, gamma: Double, offset: RGB) {
  def toColorTransform() = {
    ColorTransform(gain, gamma, offset)
  }
}

object ColorTransformLegacyFormat {
  def apply(ct: ColorTransform) = new ColorTransformLegacyFormat(ct.gain, ct.colorContrast, ct.offset)

  import RenderParameterJSONFormats.rgbMapper

  implicit val colorTransformLegacyMapper: upickle.default.ReadWriter[ColorTransformLegacyFormat] =
    upickle.default.macroRW
}
