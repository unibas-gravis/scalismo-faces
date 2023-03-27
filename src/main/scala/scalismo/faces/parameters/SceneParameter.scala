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
import scalismo.faces.image.PixelImage
import scalismo.geometry._
import scalismo.mesh._
import scalismo.faces.mesh._
import scalismo.faces.render._

/**
  * structure to represent a scene tree with pose nodes and render objects
  */
sealed trait SceneTree extends Iterable[SceneTree] {
  /**
    * get all properly transformed meshes in the scene
    *
    * @return
    */
  def worldMeshes: IndexedSeq[ColorNormalMesh3D] = {
    val objects: IndexedSeq[(Transform3D, RenderObject)] = posedObjects
    objects.map{case(pose, renderObject) => RenderObject.instance(renderObject).transform(pose)}
  }

  /**
    * flatten the tree (recursive), collect all render objects with their model-to-world transformation
    *
    * @return
    */
  def posedObjects: IndexedSeq[(Transform3D, RenderObject)] = {
    // recursive scene traversal
    def treeWalker(node: SceneTree, trafo: Transform3D): IndexedSeq[(Transform3D, RenderObject)] = {
      node match {
        case pn: PoseNode =>
          pn.children.flatMap{child => treeWalker(child, pn.pose.transform compose trafo)}
        case so: SceneObject => IndexedSeq((trafo, so.renderObject))
      }
    }
    // start traversal here at root
    treeWalker(this, Pose.neutral.transform)
  }

  /**
    * recursive tree walker for Traversable
    *
    * @param f function to execute for each node of the tree
    */
  override def foreach[U](f: (SceneTree) => U): Unit = this match {
    case pn: PoseNode =>
      f(pn)
      pn.children.foreach{f}
    case so: SceneObject =>
      f(so)
  }

  override def iterator: Iterator[SceneTree] = {

    // recursive scene traversal
    def treeWalker(node: SceneTree): Iterator[SceneTree] = {
      node match {
        case pn: PoseNode =>
          pn.children.iterator.flatMap{child => treeWalker(child)}
        case so: SceneObject => Iterator(so)
      }
    }
    treeWalker(this)
  }


}

case class PoseNode(pose: Pose, children: IndexedSeq[SceneTree]) extends SceneTree


case class SceneObject(renderObject: RenderObject) extends SceneTree

//case class SceneTree(renderObject: RenderObject, pose: Pose, children: IndexedSeq[SceneTree])

case class SceneParameter(view: ViewParameter,
                          camera: Camera,
                          illuminations: IndexedSeq[Illumination],
                          sceneTree: SceneTree,
                          imageSize: ImageSize,
                          colorTransform: scalismo.faces.parameters.ColorTransform) {

  /**
    * render a scene according to the scene parameter description
    *
    * @param buffer buffer to render into (mutated/updated! through RenderBuffer interface)
    * @return render buffer with rendered scene
    */
  def renderSceneToBuffer(buffer: RenderBuffer[RGBA]): RenderBuffer[RGBA] = {
    // color transform (nasty rgb/rgba conversion)
    val ct = colorTransform.transform
    val colT: RGBA => RGBA = (col: RGBA) => ct(col.toRGB).toRGBA

    // pixel shader for given illumination setting
    def makePixelShader(mesh: ColorNormalMesh3D): PixelShader[RGBA] = {
      val shaders = illuminations.map {
        case dl: DirectionalLight => dl.shader(mesh, view.eyePosition)
        case shl: SphericalHarmonicsLight => shl.shader(mesh)
      }
      shaders.reduce {_ + _}.map(colT)
    }

    // triangle filters for a mesh, back face culling
    def makeFilters(mesh: TriangleMesh[_3D]): TriangleId => Boolean = {
      val culling = TriangleFilters.backfaceCullingFilter(mesh, view.eyePosition)
      culling
    }

    // transform meshes to world space
    val worldMeshes = sceneTree.posedObjects.map{case(t, renderObject) =>
      RenderObject.instance(renderObject).transform(t)
    }

    // point shader: view and projection (model is handled explicitly)
    val viewProjection = camera.projection.pointShader(view.viewTransform)

    // screen transform / window transform
    val screenTransform = WindowTransform(imageSize.width, imageSize.height)

    // setup triangle filters: backface culling and outside clipping
    val culling = TriangleFilters

    // render all to buffer
    worldMeshes.foreach { mesh =>
      val triangleFilters = makeFilters(mesh.shape)
      val pixelShader = makePixelShader(mesh)
      TriangleRenderer.renderMesh(mesh.shape, triangleFilters, viewProjection, screenTransform, pixelShader, buffer)
    }

    // done, return filled buffer
    buffer
  }

  /**
    * render a scene according to the scene parameter description
    *
    * @return render buffer with rendered scene
    */
  def renderSceneToBuffer: RenderBuffer[RGBA] = {
    val buffer = ZBuffer[RGBA](imageSize.width, imageSize.height, RGBA.BlackTransparent)
    renderSceneToBuffer(buffer)
  }

  /**
    * render a scene according to the scene parameter description, produces an image
    *
    * @return rendered image of the scene
    */
  def renderScene(clearColor: RGBA = RGBA.BlackTransparent): PixelImage[RGBA] = {
    val buffer = ZBuffer[RGBA](imageSize.width, imageSize.height, clearColor)
    renderSceneToBuffer(buffer).toImage
  }


  /**
    * convert to a RenderParameter, only works for a single object with single illumination
    *
    * @return RenderParameter describing this simple scene
    */
  def toRenderParameter: Option[RenderParameter] = {
    for {
      light <- illuminations match {
        case IndexedSeq(envMap: SphericalHarmonicsLight, dirLight: DirectionalLight) => Some((envMap, dirLight))
        case _ => None
      }
      (pose, face) <- sceneTree match {
        case PoseNode(pose, IndexedSeq(SceneObject(face: MoMoInstance))) => Some((pose, face))
        case _ => None
      }
    } yield
      RenderParameter(
        pose = pose,
        view = view,
        camera = camera,
        environmentMap = light._1,
        directionalLight = light._2,
        momo = face,
        imageSize = imageSize,
        colorTransform = colorTransform)
  }
}

/**
  * Renderer for scene descriptions
  */
object SceneParameter {

  def apply(renderParameter: RenderParameter): SceneParameter = SceneParameter(
    view = renderParameter.view,
    camera = renderParameter.camera,
    illuminations = IndexedSeq(renderParameter.environmentMap, renderParameter.directionalLight),
    sceneTree = PoseNode(renderParameter.pose, IndexedSeq(SceneObject(renderParameter.momo))),
    imageSize = renderParameter.imageSize,
    colorTransform = renderParameter.colorTransform)
}