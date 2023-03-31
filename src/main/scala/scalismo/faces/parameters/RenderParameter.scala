/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.faces.parameters

import scalismo.color.RGBA
import scalismo.faces.mesh.ColorNormalMesh3D
import scalismo.faces.render.{Affine3D, PixelShader, PointShader}
import scalismo.geometry.{_3D, EuclideanVector, Point, Point2D}
import scalismo.mesh.VertexColorMesh3D

/** main parameter to describe a 3D scene setup with a face for rendering and fitting */
case class RenderParameter(pose: Pose,
                           view: ViewParameter,
                           camera: Camera,
                           environmentMap: SphericalHarmonicsLight,
                           directionalLight: DirectionalLight,
                           momo: MoMoInstance,
                           imageSize: ImageSize,
                           colorTransform: ColorTransform
) {

  /** change illumination to specified illumination */
  def withDirectionalLight(light: DirectionalLight): RenderParameter = copy(directionalLight = light)

  /** change illumination to specified illumination */
  def withEnvironmentMap(light: SphericalHarmonicsLight): RenderParameter = copy(environmentMap = light)

  /** change the face object */
  def withMoMo(momo: MoMoInstance): RenderParameter = copy(momo = momo)

  /** change the camera setting */
  def withCamera(camera: Camera): RenderParameter = copy(camera = camera)

  /** change the pose */
  def withPose(pose: Pose): RenderParameter = copy(pose = pose)

  /** change the viewing setup */
  def withView(view: ViewParameter): RenderParameter = copy(view = view)

  /** change image size (without rescaling anything!! - also refer to forImage*) */
  def withImageSize(imageSize: ImageSize): RenderParameter = copy(imageSize = imageSize)

  /** change the color transform */
  def withColorTransform(colorTransform: ColorTransform): RenderParameter = copy(colorTransform = colorTransform)

  /** remove directional light */
  def noDirectionalLight: RenderParameter = copy(directionalLight = DirectionalLight.off)

  /** remove spherical harmonics illumination */
  def noEnvironmentMap: RenderParameter = copy(environmentMap = SphericalHarmonicsLight.empty)

  /** disable color transform */
  def noColorTransform: RenderParameter = withColorTransform(ColorTransform.neutral)

  /** disable illumination, render pure albedo color */
  def noLightAndColor: RenderParameter = copy(
    colorTransform = ColorTransform.neutral,
    environmentMap = SphericalHarmonicsLight.ambientWhite,
    directionalLight = DirectionalLight.off
  )

  /** model and view transforms: pose and camera (transforms from object to eye coordinates) */
  def modelViewTransform: Affine3D = view.viewTransform compose pose.transform

  /** complete render transform, transforms from object to buffer coordinates (includes y inversion in image) */
  def renderTransform: Point[_3D] => Point[_3D] = { p => imageSize.screenTransform(pointShader(p)) }

  /** complete shader of this scene, includes point and pixel shaders (ParametricShader) */
  def pixelShader(mesh: ColorNormalMesh3D): PixelShader[RGBA] = {
    val worldMesh = mesh.transform(pose.transform)
    val ctRGB = colorTransform.transform
    val ct = (c: RGBA) => ctRGB(c.toRGB).toRGBA
    // default: both illumination sources active
    (environmentMap.shader(worldMesh, view.eyePosition) + directionalLight.shader(worldMesh, view.eyePosition)).map(ct)
    // old compatibility
    if (environmentMap.nonEmpty)
      environmentMap.shader(worldMesh, view.eyePosition).map(ct)
    else
      directionalLight.shader(worldMesh, view.eyePosition).map(ct)
  }

  /**
   * complete shader of this scene for a vertex color mesh with vertex normals, includes point and pixel shaders
   * (ParametricShader)
   */
  def pixelShader(mesh: VertexColorMesh3D): PixelShader[RGBA] = pixelShader(ColorNormalMesh3D(mesh))

  /** point shader: transform from object to canonical viewing volume/NDC */
  def pointShader: PointShader = camera.projection.pointShader(modelViewTransform)

  /** change rendering output - corresponds to a restricted or enlarged view, does not change size of head */
  def forImageSize(width: Int, height: Int): RenderParameter = {
    val sw = width.toDouble / imageSize.width
    val sh = height.toDouble / imageSize.height
    copy(
      imageSize = ImageSize(width, height),
      camera = camera.copy(sensorSize = EuclideanVector(camera.sensorSize.x * sw, camera.sensorSize.y * sh))
    )
  }

  /** adapt the target image size, rescales output, keeps aspect ratio of original face */
  def fitToImageSize(width: Int, height: Int): RenderParameter = {
    val sw = width.toDouble / imageSize.width
    val sh = height.toDouble / imageSize.height
    // find maximal scaling (to fit both sides into the new image)
    val isoScale = math.min(sw, sh)
    // additional scale necessary to adapt to new pixel ratio, extend view
    copy(imageSize = imageSize.scaleImage(isoScale)).forImageSize(width, height)
  }
}

object RenderParameter {

  /** default setting, corresponds to a full format (35mm, aspect 3/2) DSL camera with 50mm lens, object distance 1m */
  val default: RenderParameter = RenderParameter(
    pose = Pose.away1m,
    view = ViewParameter.neutral,
    camera = Camera.for35mmFilm(50.0),
    environmentMap = SphericalHarmonicsLight.frontal.withNumberOfBands(2),
    directionalLight = DirectionalLight.off,
    momo = MoMoInstance.empty,
    imageSize = ImageSize(720, 480),
    colorTransform = ColorTransform.neutral
  )

  /** old square rendering default, 1m distance */
  val defaultSquare = RenderParameter(
    pose = Pose.away1m,
    view = ViewParameter.neutral,
    camera = Camera(focalLength = 7.5,
                    principalPoint = Point2D.origin,
                    sensorSize = EuclideanVector(2.0, 2.0),
                    near = 10,
                    far = 1000e3,
                    orthographic = false
    ),
    environmentMap = SphericalHarmonicsLight.frontal.withNumberOfBands(2),
    directionalLight = DirectionalLight.off,
    momo = MoMoInstance.empty,
    imageSize = ImageSize(512, 512),
    colorTransform = ColorTransform.neutral
  )
}
