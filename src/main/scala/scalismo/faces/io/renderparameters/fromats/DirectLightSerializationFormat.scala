package scalismo.faces.io.renderparameters.fromats

import scalismo.color.RGB
import scalismo.faces.parameters.DirectionalLight
import scalismo.geometry.{_3D, EuclideanVector}

case class DirectLightSerializationFormat(
  ambient: RGB,
  diffuse: RGB,
  direction: EuclideanVector[_3D],
  specular: RGB,
  shininess: Double
) {
  def toDirectionalLight(): DirectionalLight = DirectionalLight(ambient, diffuse, direction, specular, shininess)
}

object DirectLightSerializationFormat {
  def apply(dl: DirectionalLight) =
    new DirectLightSerializationFormat(dl.ambient, dl.diffuse, dl.direction, dl.specular, dl.shininess)

  import RenderParameterJSONFormats.rgbMapper
  import RenderParameterJSONFormats.ev3DMapper

  implicit val directionalLightSerializationMapper: upickle.default.ReadWriter[DirectLightSerializationFormat] =
    upickle.default.macroRW
}
