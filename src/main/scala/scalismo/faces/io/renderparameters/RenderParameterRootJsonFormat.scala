package scalismo.faces.io.renderparameters

import scalismo.faces.parameters.RenderParameter

import upickle.default.ReadWriter

trait RenderParameterRootJsonFormat {
  val version: String = ""

  val rpsMapper: ReadWriter[RenderParameter]
}
