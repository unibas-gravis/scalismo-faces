package scalismo.faces.io.renderparameters.fromats

import scalismo.faces.parameters.MoMoInstance

import java.net.URI

case class MoMoInstanceLegacyFormat(
  colorCoefficients: IndexedSeq[Double],
  modelURI: URI,
  shapeCoefficients: IndexedSeq[Double],
  shininess: Double
) {
  def toMoMoInstance = MoMoInstance(shapeCoefficients, colorCoefficients, IndexedSeq.empty, modelURI)
}

object MoMoInstanceLegacyFormat {
  def apply(momo: MoMoInstance, shininess: Double): MoMoInstanceLegacyFormat =
    MoMoInstanceLegacyFormat(momo.color, momo.modelURI, momo.shape, shininess)

  import RenderParameterJSONFormats.uriMapper

  implicit val moMoInstanceLegacyMapper: upickle.default.ReadWriter[MoMoInstanceLegacyFormat] = upickle.default.macroRW
}
