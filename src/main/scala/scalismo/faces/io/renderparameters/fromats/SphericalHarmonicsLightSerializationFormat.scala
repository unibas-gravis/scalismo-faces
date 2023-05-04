package scalismo.faces.io.renderparameters.fromats

case class SphericalHarmonicsLightSerializationFormat(coefficients: List[List[Double]])

implicit val sphericalHarmonicsLightSerializationFormatMapper
  : upickle.default.ReadWriter[SphericalHarmonicsLightSerializationFormat] = upickle.default.macroRW
