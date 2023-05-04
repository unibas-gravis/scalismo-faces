package scalismo.faces.io.renderparameters.fromats

import java.net.URI

import scalismo.faces.parameters.MoMoInstance

case class MoMoInstanceSerializationFormat(shape: IndexedSeq[Double],
                                           color: IndexedSeq[Double],
                                           expression: IndexedSeq[Double],
                                           modelURI: URI
) {
  def toMoMo(): MoMoInstance = MoMoInstance(shape, color, expression, modelURI)
}

object MoMoInstanceSerializationFormat {
  def apply(momoInstance: MoMoInstance): MoMoInstanceSerializationFormat = new MoMoInstanceSerializationFormat(
    momoInstance.shape,
    momoInstance.color,
    momoInstance.expression,
    momoInstance.modelURI
  )

  import RenderParameterJSONFormats.uriMapper

  implicit val momoInstanceSerializationMapper: upickle.default.ReadWriter[MoMoInstanceSerializationFormat] =
    upickle.default.macroRW
}
