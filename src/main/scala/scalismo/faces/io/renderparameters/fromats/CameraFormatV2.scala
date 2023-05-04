package scalismo.faces.io.renderparameters.fromats

import scalismo.geometry.{_2D, _3D, EuclideanVector, Point}
import scalismo.faces.parameters.{Camera, ImageSize, ViewParameter}

case class CameraFormatV2(far: Double,
                          focalLength: Double,
                          near: Double,
                          orthographic: Boolean,
                          pitch: Double,
                          principalPoint: EuclideanVector[_2D],
                          roll: Double,
                          sensorSize: EuclideanVector[_2D],
                          skewFactor: Double,
                          translation: EuclideanVector[_3D],
                          yaw: Double
) {
  def toCamera(imageSize: ImageSize): Camera = {
    val pp = Point(2 * principalPoint.x / imageSize.width, 2 * principalPoint.y / imageSize.height)
    Camera(
      focalLength,
      pp,
      sensorSize,
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

object CameraFormatV2 {
  def apply(cam: Camera, imageSize: ImageSize, view: ViewParameter): CameraFormatV2 = {
    val pp = Point(0.5 * imageSize.width * cam.principalPoint.x, 0.5 * imageSize.height * cam.principalPoint.y)
    new CameraFormatV2(
      far = cam.far,
      focalLength = cam.focalLength,
      near = cam.near,
      orthographic = cam.orthographic,
      pitch = view.pitch,
      principalPoint = pp.toVector,
      roll = view.roll,
      sensorSize = cam.sensorSize,
      skewFactor = 1.0,
      translation = view.translation,
      yaw = view.yaw
    )
  }

  import RenderParameterJSONFormats.ev2DMapper
  import RenderParameterJSONFormats.ev3DMapper

  implicit val cameraV2Mapper: upickle.default.ReadWriter[CameraFormatV2] = upickle.default.macroRW
}
