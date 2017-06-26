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

package scalismo.faces.manipulation

import scalismo.faces.color._
import scalismo.faces.image._
import scalismo.faces.mesh.VertexColorMesh3D
import scalismo.faces.momo.{MoMo, MoMoCoefficients}
import scalismo.faces.parameters.{ParametricRenderer, RenderParameter}
import scalismo.geometry._
import scalismo.mesh.{SurfacePointProperty, TriangleMesh3D}

object FaceManipulation {
  def manipulateShape(shape: IndexedSeq[Point[_3D]], model: MoMo, vector: MoMoCoefficients, strength: Double): IndexedSeq[Point[_3D]] = {
    require(shape.size == model.mean.shape.pointSet.numberOfPoints, "(shape) can only manipulate meshes compatible with the model")
    // get manipulated mean
    val mean = model.mean
    val manipulatedMean = model.instance(vector)
    // extract difference
    val diff = mean.shape.pointSet.points.zip(manipulatedMean.shape.pointSet.points).map { case (o, m) => m - o }.toIndexedSeq
    // apply to shape
    shape.zip(diff).map { case (s, d) => s + strength *: d }
  }

  def manipulateColor(color: IndexedSeq[RGBA], model: MoMo, vector: MoMoCoefficients, strength: Double): IndexedSeq[RGBA] = {
    require(color.size == model.mean.shape.pointSet.numberOfPoints, "(color) can only manipulate meshes compatible with the model")
    // get manipulated mean
    val mean = model.mean
    val manipulatedMean = model.instance(vector)
    // extract difference
    val diff = for (i <- mean.color.triangulation.pointIds) yield manipulatedMean.color.atPoint(i) - mean.color.atPoint(i)
    // apply to color
    for (i <- color.indices) yield color(i) + strength *: diff(i)
  }

  def manipulateColorMesh(mesh: VertexColorMesh3D, model: MoMo, vector: MoMoCoefficients, strengthShape: Double, strengthColor: Double): VertexColorMesh3D = {
    val triangulation = mesh.shape.triangulation
    val manipulatedColor = manipulateColor(mesh.color.pointData, model, vector, strengthColor)

    VertexColorMesh3D(
      manipulateShapeMesh(mesh.shape, model, vector, strengthShape),
      SurfacePointProperty(triangulation, manipulatedColor))
  }

  def manipulateShapeMesh(mesh: TriangleMesh3D, model: MoMo, vector: MoMoCoefficients, strengthShape: Double): TriangleMesh3D = {
    val triangulation = mesh.triangulation
    val manipulatedShape = manipulateShape(mesh.pointSet.points.map(p => p: Point[_3D]).toIndexedSeq, model, vector, strengthShape)

    TriangleMesh3D(manipulatedShape, triangulation)
  }

  /** Renders a parameter space manipulation. */
  def renderManipulation(fitParameter: RenderParameter,
                         fitInstance: VertexColorMesh3D,
                         targetImage: PixelImage[RGBA],
                         model: MoMo,
                         manipulationVector: MoMoCoefficients,
                         warpFieldExtrapolator: WarpExtrapolator,
                         colorTransfer: (RGBA, RGBA, RGBA) => RGBA = additiveTransfer): PixelImage[RGBA] = {
    val manipulatedMesh: VertexColorMesh3D = FaceManipulation.manipulateColorMesh(fitInstance, model, manipulationVector, 1f, 1f)
    renderManipulationMeshBased(fitParameter, fitInstance, manipulatedMesh, targetImage, warpFieldExtrapolator, colorTransfer)
  }

  /** render a mesh manipulation. */
  def renderManipulationMeshBased(fitParameter: RenderParameter,
                                  targetMesh: VertexColorMesh3D,
                                  manipulatedMesh: VertexColorMesh3D,
                                  targetImage: PixelImage[RGBA],
                                  warpFieldExtrapolator: WarpExtrapolator,
                                  colorTransfer: (RGBA, RGBA, RGBA) => RGBA = additiveTransfer): PixelImage[RGBA] = {
    require(fitParameter.imageSize.domain == targetImage.domain, "parameter size does not match image size")

    // manipulation warp field
    val warpFieldManipulation = {
      manipulationWarpField(fitParameter,
        fitParameter,
        targetMesh.shape,
        manipulatedMesh.shape)
    }

    val warpFieldHard = ConstantWarpExtrapolator(Vector(0f, 0f))(warpFieldManipulation)
    val warpField = warpFieldExtrapolator(warpFieldManipulation)

    // warp original image
    val warpedImage = ImageWarper.warpImage(targetImage.withAccessMode(AccessMode.Padded(RGBA.WhiteTransparent)), warpField)

    // warped source rendering
    val sourceRendering = ParametricRenderer.renderParameterVertexColorMesh(fitParameter, targetMesh)
    // pixels at border get interpolated values with background, prevent this using None for outside values:
    // keep only results which depend only on inside pixels
    val warpedSourceRendering = ImageWarper.warpImage(
      sourceRendering.map(p => if (p.a > 0.99f) Some(p) else None),
      warpFieldHard)
      .map(v => v.getOrElse(RGBA.WhiteTransparent))

    // target rendering
    val targetRendering = ParametricRenderer.renderParameterVertexColorMesh(fitParameter, manipulatedMesh)
    // difference transfer on original image
    val diffImage = PixelImage(targetRendering.domain, (x, y) => RGBA((targetRendering(x, y) - warpedSourceRendering(x, y)).toRGB, targetRendering(x, y).a))

    val manipImage = PixelImage(warpedImage.domain, (x, y) => colorTransfer(warpedImage(x, y), warpedSourceRendering(x, y), targetRendering(x, y)))

    val mask = PixelImage(targetRendering.domain, (x, y) => targetRendering(x, y).a * warpedSourceRendering(x, y).a)

    // blend images: Poisson inpainting
    val maskedManipImage = PixelImage(manipImage.domain, (x, y) => if (mask(x, y) > 0.99f) Some(manipImage(x, y)) else None)

    val finalImage = {
      val inpainter = new PoissonInpainting[RGBA](new GenericMultigridPoissonSolver[RGBA])
      inpainter.seamlessCloning(warpedImage, maskedManipImage)
    }

    // DEBUG
    //    PixelImageIO.write(WarpFieldVisualizer.renderWarpFieldColor(warpFieldHard), new File("/tmp/manip/wf-hard.png")).get
    //    PixelImageIO.write(WarpFieldVisualizer.renderWarpFieldColor(warpField), new File("/tmp/manip/wf-smooth.png")).get
    //    PixelImageIO.write(sourceRendering,       new File("/tmp/manip/sourceRendering.png")).get
    //    PixelImageIO.write(warpedSourceRendering, new File("/tmp/manip/sourceRenderingWarped.png")).get
    //    PixelImageIO.write(targetRendering,       new File("/tmp/manip/targetRendering.png")).get
    //    PixelImageIO.write(diffImage.map(p => p.mapRGB(_ + RGB(0.5f))),             new File("/tmp/manip/diffImage.png")).get
    //    PixelImageIO.write(warpedImage,           new File("/tmp/manip/warpedImage.png")).get
    //    PixelImageIO.write(manipImage,            new File("/tmp/manip/manipImage.png")).get
    //    PixelImageIO.write(finalImage,            new File("/tmp/manip/finalImage.png")).get
    finalImage
  }

  def additiveTransfer(targetColor: RGBA, sourceRendering: RGBA, targetRendering: RGBA): RGBA = {
    RGBA(targetColor.toRGB + (targetRendering.toRGB - sourceRendering.toRGB), targetColor.a * sourceRendering.a)
  }

  def multiplicativeTransfer(targetColor: RGBA, sourceRendering: RGBA, targetRendering: RGBA): RGBA = {
    targetColor x (targetRendering / sourceRendering.map(f => math.max(1e-5f, f)))
  }

  /** render an image manipulation warp */
  def manipulationWarpField(sourceParameter: RenderParameter,
                            targetParameter: RenderParameter,
                            source: TriangleMesh3D,
                            target: TriangleMesh3D): PixelImage[Option[Vector[_2D]]] = {
    // render both to 2d
    val points2DOriginal = source.pointSet.points.map(p => sourceParameter.renderTransform(p)).map(p => Point(p.x, p.y)).toIndexedSeq
    val points2DManipulated = target.pointSet.points.map(p => targetParameter.renderTransform(p)).map(p => Point(p.x, p.y)).toIndexedSeq

    val warpList: IndexedSeq[Vector[_2D]] = points2DOriginal.zip(points2DManipulated).map { case (o, m) => o - m }

    // render warp field
    ParametricRenderer.renderPropertyImage(
      targetParameter,
      target,
      SurfacePointProperty(target.triangulation, warpList)
    )
  }
}