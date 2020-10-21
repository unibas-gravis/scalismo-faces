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

import java.io.File
import java.net.URI
import java.util.Map.Entry

import scalismo.faces.io.MoMoIO
import scalismo.faces.io.msh.MSHMeshIO
import scalismo.faces.mesh.ColorNormalMesh3D
import scalismo.faces.momo.{MoMo, MoMoCoefficients}
import scalismo.mesh.VertexColorMesh3D
import scalismo.utils.Random

import scala.util.Try

/** parametric description of a renderable object */
sealed trait RenderObject


object RenderObject {
  object MeshCache {
    /** cache to hold open models, identified by their URL */
    private val cacheSizeHint = 10
    private val openMeshes = new java.util.LinkedHashMap[URI, Object](cacheSizeHint, 0.75f, false) {
      override def removeEldestEntry(eldest: Entry[URI, Object]): Boolean = size() > cacheSizeHint
    }

    /**
      * clears all cached meshes
      * */
    def clearURICache(): Unit = openMeshes.synchronized( openMeshes.clear() )

    /**
      * load a mesh from an URI (cached)
      *
      * @param uri URI of model to open, must be a file currently
      * @return
      */
    def loadMesh(uri: URI): Try[ColorNormalMesh3D] = Try {
      // look in cache
      val meshLookUp: Option[(ColorNormalMesh3D, Long)] =
        openMeshes.synchronized {
          val meshObj: Option[Object] = Option(openMeshes.get(uri))
          meshObj.map{obj => obj.asInstanceOf[(ColorNormalMesh3D, Long)]}
        }.flatMap{ (stampedMesh: (ColorNormalMesh3D, Long)) =>
          if (timestamp(uri) <= stampedMesh._2)
            Some(stampedMesh)
          else
            None
        }

      // open if necessary
      meshLookUp.map{_._1}.getOrElse {
        val mesh = forceLoad(uri).get
        openMeshes.synchronized {
          openMeshes.put(uri, mesh)
        }
        mesh._1
      }
    }

    private def timestamp(uri: URI): Long = new File(uri).lastModified()

    private def forceLoad(uri: URI): Try[(ColorNormalMesh3D, Long)] = Try {
      val file = new File(uri)
      (MSHMeshIO.read(file).get.colorNormalMesh.get, timestamp(uri))
    }

  }

  def instance(renderObject: RenderObject): ColorNormalMesh3D = {
    renderObject match {
      case MeshFile(meshURI) =>
        MeshCache.loadMesh(meshURI).get
      case momo: MoMoInstance =>
        val model = MoMoIO.read(momo.modelURI).get
        val m = model.instance(momo.coefficients)
        ColorNormalMesh3D(m.shape, m.color, m.shape.vertexNormals)
      case MeshColorNormals(mesh) => mesh
      case MeshVertexColor(mesh) => ColorNormalMesh3D(mesh)
    }
  }
}

/** parameters of a Morphable Model instance, consists of shape and color */
case class MoMoInstance(shape: IndexedSeq[Double],
                        color: IndexedSeq[Double],
                        expression: IndexedSeq[Double],
                        modelURI: URI) extends RenderObject {

  /** coefficients as MoMoCoefficients, compatible with MoMo class */
  def coefficients = MoMoCoefficients(shape, color, expression)

  /** resize coefficient vectors (fills with 0 or drops) */
  def withNumberOfCoefficients(nShape: Int, nColor: Int, nExpress: Int): MoMoInstance = {
    def ensureLength(seq: IndexedSeq[Double], n: Int): IndexedSeq[Double] = {
      if (seq.length > n) seq.slice(0, n)
      else if (seq.length < n) seq ++ IndexedSeq.fill(n - seq.length)(0.0)
      else seq
    }
    val newShape = ensureLength(shape, nShape)
    val newColor = ensureLength(color, nColor)
    val newExpress = ensureLength(expression, nExpress)
    copy(shape = newShape, color = newColor, expression = newExpress)
  }
}

object MoMoInstance {
  val empty = MoMoInstance(IndexedSeq.empty[Double], IndexedSeq.empty[Double], IndexedSeq.empty[Double], new URI(""))

  /** create a MoMoInstance from a set of MoMoCoefficients */
  def fromCoefficients(momoCoefficients: MoMoCoefficients, modelURI: URI): MoMoInstance = {
    new MoMoInstance(
      momoCoefficients.shape.toArray.toIndexedSeq,
      momoCoefficients.color.toArray.toIndexedSeq,
      momoCoefficients.expression.toArray.toIndexedSeq,
      modelURI)
  }

  /** create a zero MoMoInstance (usually corresponds to mean) matching a model's rank */
  def zero(model: MoMo, modelURI: URI): MoMoInstance = fromCoefficients(model.zeroCoefficients, modelURI)

  /** create a zero MoMoInstance (usually corresponds to mean) of specified size */
  def zero(shapeComponents: Int,
           colorComponents: Int,
           expressionComponents: Int,
           modelURI: URI): MoMoInstance = {
    fromCoefficients(MoMoCoefficients.zeros(shapeComponents, colorComponents, expressionComponents), modelURI)
  }

  /** create a random coefficient vector with its elements N(0,1) distributed. */
  def sample(model: MoMo, modelURI: URI)(implicit rnd: Random): MoMoInstance = fromCoefficients(model.sampleCoefficients(), modelURI)
}

/** mesh file reference to render */
case class MeshFile(meshURI: URI) extends RenderObject

/** direct mesh to render with vertex color */
case class MeshColorNormals(colorNormalMesh: ColorNormalMesh3D) extends RenderObject

/** direct mesh to render with vertex color */
case class MeshVertexColor(vertexColorMesh3D: VertexColorMesh3D) extends RenderObject
