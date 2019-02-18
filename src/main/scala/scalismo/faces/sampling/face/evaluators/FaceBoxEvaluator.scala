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

package scalismo.faces.sampling.face.evaluators

import java.io.{File, InputStream}

import scalismo.faces.parameters.RenderParameter
import scalismo.faces.sampling.face.ParametricLandmarksRenderer
import scalismo.geometry.{Point, EuclideanVector, _2D}
import scalismo.sampling.DistributionEvaluator

import scala.io.Source
import scala.util.Try


/**
  * evaluate face position with respect to a given detection face box (has scale and position)
  * @param renderer parametric landmarks renderer, needs to know Seq("center.nose.tip", "left.eye.corner_outer", "right.eye.corner_outer", "right.lips.corner", "left.lips.corner")
  * @param faceBox target face box
  * @param scaleEvaluator evaluates scale mismatch (as relative factor, 1.0 is a perfect match)
  * @param positionEvaluator evaluates position mismatch (as offset, EuclideanVector(0, 0) is a perfect match) */
class FaceBoxEvaluator(renderer: ParametricLandmarksRenderer,
                       faceBox: FaceBox,
                       scaleEvaluator: DistributionEvaluator[Double],
                       positionEvaluator: DistributionEvaluator[EuclideanVector[_2D]])
  extends DistributionEvaluator[RenderParameter] {

  // lmIds to estimate face scale
  val lmIds = Seq("center.nose.tip", "left.eye.corner_outer", "right.eye.corner_outer", "right.lips.corner", "left.lips.corner")

  // render scale and position of face
  def renderScaleAndCenter(parameter: RenderParameter): (Double, Point[_2D]) = {
    val renderedLM = lmIds.map{id => id -> renderer.renderLandmark(id, parameter).get}.toMap

    val faceCenter = renderedLM("center.nose.tip").point

    val eyeDistance = (renderedLM("right.eye.corner_outer").point - renderedLM("left.eye.corner_outer").point).norm
    val rightEyeLipDistance = (renderedLM("right.eye.corner_outer").point - renderedLM("right.lips.corner").point).norm
    val leftEyeLipDistance = (renderedLM("left.eye.corner_outer").point - renderedLM("left.lips.corner").point).norm
    val faceScale = math.max(eyeDistance, math.max(rightEyeLipDistance, leftEyeLipDistance))

    (faceScale, faceCenter)
  }

  // evaluate with respect to target face box, scale and position
  override def logValue(sample: RenderParameter): Double = {
    val (faceScale, faceCenter) = renderScaleAndCenter(sample)
    val scaleValue = scaleEvaluator.logValue(faceScale / faceBox.scale)
    val positionValue = positionEvaluator.logValue(faceCenter - faceBox.center)
    scaleValue + positionValue
  }
}

/** a face detection candidate */
case class FaceBox(topLeft: Point[_2D], bottomRight: Point[_2D], certainty: Double) {
  val size: EuclideanVector[_2D] = bottomRight - topLeft
  val center: Point[_2D] = topLeft + 0.5 *: size
  val scale: Double = size.norm / 3.0
}

object FaceBox {
  def fromYAML(yaml: String): Try[FaceBox] = Try {
    // read file line by line
    val lines = Source.fromString(yaml).getLines()
    // extract position and scale
    var boxPosition: Option[(Point[_2D], EuclideanVector[_2D])] = None
    var certainty: Option[Double] = None
    // extraction patterns
    val boxMatcher = "FaceBox:\\[([0-9.]+),([0-9.]+),([0-9.]+),([0-9.]+)\\]".r
    val certaintyMatcher = """pFace:([-+]?[0-9]*\.?([0-9]+)?([eE][-+]?[0-9]+)?)""".r
    // process line after line
    for (l <- lines) {
      val trimmed = """ """.r.replaceAllIn(l.trim, "")
      trimmed match {
        case boxMatcher(tlx, tly, sx, sy) =>
          val topLeft = Point(tlx.toDouble, tly.toDouble)
          val size = EuclideanVector(sx.toDouble, sy.toDouble)
          boxPosition = Some(topLeft, size)
        case certaintyMatcher(c, _, _) =>
          certainty = Some(c.toDouble)
        case li: String => Unit
      }
    }
    // get results
    val (tl, size) = boxPosition.getOrElse(throw new RuntimeException("no FaceBox found in file"))
    val cert = certainty.getOrElse(throw new RuntimeException("no certainty found in file"))
    // assemble face box
    FaceBox(tl, tl + size, cert)
  }

  def fromYAMLStream(stream: InputStream): Try[FaceBox] = Try {
    fromYAML(Source.fromInputStream(stream).mkString).get
  }

  /** read face box from standard map file format: YAML with "faceBox: [topLeft.x, topLeft.y, bottomRight.x, bottomRight.y]" and "pValue: certainty" */
  def fromYAMLFile(file: File): Try[FaceBox] = Try {
    fromYAML(Source.fromFile(file).mkString).get
  }
}