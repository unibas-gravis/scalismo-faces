package scalismo.faces.io.renderparameters.fromats

import scalismo.geometry.{_2D, _3D, EuclideanVector, Point}
import scalismo.faces.parameters.Camera
import scalismo.faces.parameters.ImageSize
import scalismo.faces.parameters.ViewParameter

case class LegacyCameraFormat(far: Double,
                              focalLength: Double,
                              near: Double,
                              orthographic: Boolean,
                              pitch: Double,
                              principlePoint: EuclideanVector[_2D],
                              roll: Double,
                              sensorSize: EuclideanVector[_2D],
                              skewFactor: Double,
                              translation: EuclideanVector[_3D],
                              yaw: Double
) {
  def toCamera(imageSize: ImageSize): Camera = {
    val pp = Point(2 * principlePoint.x / imageSize.width, 2 * principlePoint.y / imageSize.height)
    Camera(
      focalLength,
      pp,
      sensorSize / near,
      near = near,
      far = far,
      orthographic = orthographic
    )
  }

  def toView: ViewParameter = ViewParameter(
    translation = translation,
    pitch = pitch,
    yaw = yaw,
    roll = roll
  )
}

object LegacyCameraFormat {
  def apply(cam: Camera, imageSize: ImageSize, view: ViewParameter): LegacyCameraFormat = {
    val pp = Point(0.5 * imageSize.width * cam.principalPoint.x, 0.5 * imageSize.height * cam.principalPoint.y)
    new LegacyCameraFormat(
      far = cam.far,
      focalLength = cam.focalLength,
      near = cam.near,
      orthographic = cam.orthographic,
      pitch = view.pitch,
      principlePoint = pp.toVector,
      roll = view.roll,
      sensorSize = cam.sensorSize * cam.near,
      skewFactor = 1.0,
      translation = view.translation,
      yaw = view.yaw
    )
  }

  import RenderParameterJSONFormats.ev2DMapper
  import RenderParameterJSONFormats.ev3DMapper

  implicit val legacyCameraMapper: upickle.default.ReadWriter[LegacyCameraFormat] = upickle.default.macroRW
}
