package scalismo.faces.io.renderparameters.fromats

import scalismo.color.RGB
import scalismo.faces.parameters.DirectionalLight
import scalismo.geometry.{_3D, EuclideanVector}

case class DirectLightLegacySerializationFormat(
  ambient: RGB,
  diffuse: RGB,
  direction: EuclideanVector[_3D],
  specular: RGB
) {
  def toDirectionalLight(shininess: Double): DirectionalLight =
    DirectionalLight(ambient, diffuse, direction, specular, shininess)
}

object DirectLightLegacySerializationFormat {
  def apply(dl: DirectionalLight) =
    new DirectLightLegacySerializationFormat(dl.ambient, dl.diffuse, dl.direction, dl.specular)

  import RenderParameterJSONFormats.{ev3DMapper, rgbMapper}

  implicit val directionalLightSerializationMapper: upickle.default.ReadWriter[DirectLightLegacySerializationFormat] =
    upickle.default.macroRW
}
