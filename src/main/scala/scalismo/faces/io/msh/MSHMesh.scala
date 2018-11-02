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

package scalismo.faces.io.msh

import java.io.File

import scalismo.color.RGBA
import scalismo.common.PointId
import scalismo.faces.color.ColorBlender
import scalismo.faces.image.{PixelImage, PixelImageOperations}
import scalismo.faces.io.PixelImageIO
import scalismo.faces.mesh._
import scalismo.geometry.{IntVector, Point, Vector, _2D, _3D}
import scalismo.mesh._

/** capture the structure of the Gravis MSH mesh (do not use in production unless you really need to) */
case class MSHMesh(materials: Array[MSHMaterial],
                   vertex: Array[Point[_3D]],
                   normal: Array[Vector[_3D]],
                   textureCoordinates: Array[Point[_3D]],
                   color: Array[RGBA],
                   tvi: Array[IntVector[_3D]],
                   tni: Array[IntVector[_3D]],
                   tti: Array[IntVector[_3D]],
                   tci: Array[IntVector[_3D]],
                   tmi: Array[Int],
                   lvi: Array[IntVector[_2D]],
                   lti: Array[IntVector[_2D]],
                   lci: Array[IntVector[_2D]],
                   pvi: Array[Int],
                   pci: Array[Int],
                   path: File) extends Cloneable {

  val triangles: Array[TriangleCell] = tvi.map(i => TriangleCell(PointId(i(0)), PointId(i(1)), PointId(i(2))))
  val triangulation = TriangleList(triangles)

  val triangleMesh: TriangleMesh3D = TriangleMesh3D(vertex, triangulation)

  lazy val vertexColorMesh: Option[VertexColorMesh3D] = getVertexColor.map(vc =>
    VertexColorMesh3D(triangleMesh, SurfacePointProperty.averagedPointProperty(vc))
  )

  lazy val colorNormalMesh: Option[ColorNormalMesh3D] = {
    for {
      color <- getColor
    } yield {
      val normals = getNormal.getOrElse(triangleMesh.vertexNormals)
      ColorNormalMesh3D(triangleMesh, color, normals)
    }
  }

  def getNormal: Option[VertexPropertyPerTriangle[Vector[_3D]]] = {
    val normalsValid = tni.forall(i => normal.isDefinedAt(i(0)) && normal.isDefinedAt(i(1)) && normal.isDefinedAt(i(2)))
    if (normal.nonEmpty && tni.nonEmpty && normalsValid)
      Some(VertexPropertyPerTriangle(triangulation, tni, normal))
    else
      None
  }

  /** color of this Mesh, if texture is set use it, vertex color otherwise */
  def getColor: Option[MeshSurfaceProperty[RGBA]] = {
    val tex = getSingleTextureColor
    val vc = getVertexColor
    tex.orElse(vc)
  }

  def getVertexColor: Option[VertexPropertyPerTriangle[RGBA]] = {
    if (color.nonEmpty)
      Some(VertexPropertyPerTriangle(triangulation, tci, color))
    else
      None
  }

  def getSingleTextureColor(implicit pointBlender: ColorBlender[Point[_2D]], colorBlender: ColorBlender[RGBA]): Option[TextureMappedProperty[RGBA]] = {
    if (textureCoordinates.nonEmpty) {
      /* stitch textures */
      // keep None in list to keep tmi indices valid, must not remove materials
      val textures: IndexedSeq[Option[MSHTexture]] = materials.toIndexedSeq.map(m => m.texture)
      val textureImages = for (t <- textures) yield t.map(tex => tex.image)
      val widths = for (t <- textureImages) yield t.map(i => i.width)
      val flatWidths = for (w <- widths) yield w.getOrElse(0) // replace None with 0 width (will not be in stitched texture)
      val heights = for (t <- textureImages) yield t.map(i => i.height)
      val flatHeights = for (h <- heights) yield h.getOrElse(0) // replace None with 0 width (will not be in stitched texture)
      val xOffsets = flatWidths.scanLeft(0)(_ + _) // cumsum: additive offsets
      val masterTexture = PixelImageOperations.stitchHorizontal(textureImages.flatten)

      // build new texture coordinates, depending on material, shift to match masterTexture
      /* remap texture coordinates to large single texture image */
      def remapTextureCoordinates(m: Int, coords: Point[_2D]): Point[_2D] = {
        val x = coords(0) * flatWidths(m)
        val x1 = x + xOffsets(m)
        val relX1 = x1 / masterTexture.width
        val relY1 = coords(1) * flatHeights(m) / masterTexture.height
        Point(relX1, relY1)
      }

      val masterTextCoords = Array.fill(textureCoordinates.length)(Point(0.0f, 0.0f))
      def p2d(p: Point[_3D]) = Point(p(0), p(1))
      for (t <- triangles.indices) {
        val t1 = textureCoordinates(tti(t)(0))
        val t2 = textureCoordinates(tti(t)(1))
        val t3 = textureCoordinates(tti(t)(2))
        masterTextCoords(tti(t)(0)) = remapTextureCoordinates(tmi(t), p2d(t1))
        masterTextCoords(tti(t)(1)) = remapTextureCoordinates(tmi(t), p2d(t2))
        masterTextCoords(tti(t)(2)) = remapTextureCoordinates(tmi(t), p2d(t3))
      }

      val textureMapping: VertexPropertyPerTriangle[Point[_2D]] = VertexPropertyPerTriangle(triangulation, tti, masterTextCoords)
      Some(TextureMappedProperty(triangulation, textureMapping, masterTexture.buffer))
    } else
      None
  }

  def getTextureColor(nonTexturedColor: RGBA = RGBA.WhiteTransparent)(implicit pointBlender: ColorBlender[Point[_2D]], blender: ColorBlender[RGBA]): Option[IndirectProperty[RGBA]] = {
    if (textureCoordinates.nonEmpty) {
      // get all textures
      val textures: IndexedSeq[Option[MSHTexture]] = materials.map(m => m.texture)
      // build TextureMappedSurfaceProperty for each, using the same texture coordinates (textureMapping)
      val textureMapping = VertexPropertyPerTriangle(triangulation, tti, textureCoordinates.map(p => Point(p.x, p.y)))
      // create the indirected (with tmi) texture property
      val textureProperties: IndexedSeq[Option[TextureMappedProperty[RGBA]]] = for (t: Option[MSHTexture] <- textures) yield t.map(tex => TextureMappedProperty(triangulation, textureMapping, tex.image))
      // replace all non-textured materials with a constant transparent color
      val notTextured = ConstantProperty(triangulation, nonTexturedColor)
      // setup indirection with tmi
      val surfaceProperties: Array[MeshSurfaceProperty[RGBA]] = textureProperties.map(_.getOrElse(notTextured)).toArray
      Some(IndirectProperty(triangulation, tmi, surfaceProperties))
    } else
      None
  }

  def getAmbientMaterial(implicit colorBlender: ColorBlender[RGBA]): Option[IndirectProperty[RGBA]] = {
    // create an indirect property mapping all materials
    if (materials.nonEmpty) {
      val ambientMaterials = materials.map(m => ConstantProperty(triangulation, m.ambient))
      Some(IndirectProperty(triangulation, tmi, ambientMaterials))
    } else
      None
  }

  def getDiffuseMaterial(implicit colorBlender: ColorBlender[RGBA]): Option[IndirectProperty[RGBA]] = {
    // create an indirect property mapping all materials
    if (materials.nonEmpty) {
      val diffuseMaterials = materials.map(m => ConstantProperty(triangulation, m.diffuse))
      Some(IndirectProperty(triangulation, tmi, diffuseMaterials))
    } else
      None
  }

  def getSpecularMaterial(implicit colorBlender: ColorBlender[RGBA]): Option[IndirectProperty[RGBA]] = {
    // create an indirect property mapping all materials
    if (materials.nonEmpty) {
      val specularMaterials = materials.map(m => ConstantProperty(triangulation, m.specular))
      Some(IndirectProperty(triangulation, tmi, specularMaterials))
    } else
      None
  }

  def getShininess(implicit doubleBlender: ColorBlender[Double]): Option[IndirectProperty[Double]] = {
    // create an indirect property mapping all materials
    if (materials.nonEmpty) {
      val shininess = materials.map(m => ConstantProperty(triangulation, m.shininess))
      Some(IndirectProperty(triangulation, tmi, shininess))
    } else
      None
  }

  override def equals(other: Any): Boolean = {
    def sameArray[A](a1: IndexedSeq[A], a2: IndexedSeq[A]): Boolean = a1 == a2
    other match {
      case mesh: MSHMesh =>
        sameArray(mesh.materials, materials) &&
          sameArray(mesh.vertex, vertex) &&
          sameArray(mesh.normal, normal) &&
          sameArray(mesh.textureCoordinates, mesh.textureCoordinates) &&
          sameArray(mesh.color, color) &&
          sameArray(mesh.tvi, tvi) &&
          sameArray(mesh.tci, tci) &&
          sameArray(mesh.tni, tni) &&
          sameArray(mesh.tti, tti) &&
          sameArray(mesh.tmi, tmi) &&
          sameArray(mesh.lci, lci) &&
          sameArray(mesh.lti, lti) &&
          sameArray(mesh.lvi, lvi) &&
          sameArray(mesh.pvi, pvi) &&
          sameArray(mesh.pci, pci) &&
          mesh.path == path
      case _ => false
    }
  }

  override def clone(): MSHMesh = {
    MSHMesh(
      materials.clone(),
      vertex.clone(),
      normal.clone(),
      textureCoordinates.clone(),
      color.clone(),
      tvi.clone(),
      tni.clone(),
      tti.clone(),
      tci.clone(),
      tmi.clone(),
      lvi.clone(),
      lti.clone(),
      lci.clone(),
      pvi.clone(),
      pci.clone(),
      path
    )
  }
}


object MSHMesh {
  def fromTriangleMesh3D(mesh: TriangleMesh3D, color: Option[MeshSurfaceProperty[RGBA]], normals: Option[MeshSurfaceProperty[Vector[_3D]]]): MSHMesh = color match {
    case Some(tex: TextureMappedProperty[RGBA]) => fromGravisMesh3DWithSingleTexture(mesh, tex, normals)
    case Some(tex: IndirectProperty[RGBA]) => fromGravisMesh3DWithTextures(mesh, tex, normals)
    case Some(col: MeshSurfaceProperty[RGBA]) => fromGravisMesh3DWithVertexColor(mesh, col, normals)
    case None => fromGravisMesh3DNoColors(mesh, normals)
  }

  def fromGravisMesh3DNoColors(mesh: TriangleMesh3D, normals: Option[MeshSurfaceProperty[Vector[_3D]]]): MSHMesh = {
    val triangulation = mesh.triangulation

    // normals
    val mshNormals = normals.map(n => VertexPropertyPerTriangle.fromSurfaceProperty(n))
    val tni: IndexedSeq[IntVector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.triangleVertexIndex).getOrElse(IndexedSeq.empty[IntVector[_3D]])
    val normalData: Array[Vector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.vertexData.toArray).getOrElse(Array.empty[Vector[_3D]])

    MSHMesh(
      Array(MSHMaterial.defaultMaterial),
      mesh.pointSet.points.map(p => p: Point[_3D]).toArray,
      normalData,
      Array.empty[Point[_3D]],
      Array.empty[RGBA],
      triangulation.triangles.map(_.toIntVector3D).toArray,
      tni.toArray,
      Array.empty[IntVector[_3D]],
      Array.empty[IntVector[_3D]],
      Array.fill(triangulation.triangleIds.size)(0),
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[Int],
      Array.empty[Int],
      new File(""))
  }

  def fromGravisMesh3DWithSingleTexture(mesh: TriangleMesh3D, texture: TextureMappedProperty[RGBA], normals: Option[MeshSurfaceProperty[Vector[_3D]]]): MSHMesh = {
    val triangulation = mesh.triangulation
    // create material with texture
    val material = MSHMaterial.defaultMaterialWithTexture(MSHTexture(texture.texture, new File("tex.png")))

    val texPerTriangle: VertexPropertyPerTriangle[Point[_2D]] = VertexPropertyPerTriangle.fromSurfaceProperty(texture.textureMapping)
    val tti: IndexedSeq[IntVector[_3D]] = texPerTriangle.triangleVertexIndex
    val texCoordsData: Array[Point[_3D]] = texPerTriangle.vertexData.toArray.map(p => Point(p.x, p.y, 0.0f))

    // normals
    val mshNormals = normals.map(n => VertexPropertyPerTriangle.fromSurfaceProperty(n))
    val tni: IndexedSeq[IntVector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.triangleVertexIndex).getOrElse(IndexedSeq.empty[IntVector[_3D]])
    val normalData: Array[Vector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.vertexData.toArray).getOrElse(Array.empty[Vector[_3D]])

    MSHMesh(
      Array(material),
      mesh.pointSet.points.map(p => p: Point[_3D]).toArray,
      normalData,
      texCoordsData,
      Array.empty[RGBA],
      triangulation.triangles.map(_.toIntVector3D).toArray,
      tni.toArray,
      tti.toArray,
      Array.empty[IntVector[_3D]],
      Array.fill(triangulation.triangleIds.size)(0),
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[Int],
      Array.empty[Int],
      new File(""))
  }

  def fromGravisMesh3DWithTextures(mesh: TriangleMesh3D, indirectTexture: IndirectProperty[RGBA], normals: Option[MeshSurfaceProperty[Vector[_3D]]]): MSHMesh = {
    val triangulation = mesh.triangulation

    // create material with texture
    def createTextureMaterial(texture: TextureMappedProperty[RGBA], index: Int): MSHMaterial = {
      MSHMaterial.defaultMaterialWithTexture(MSHTexture(texture.texture, new File(s"tex_$index.png")))
    }

    // we have to assume a single texture coordinate set for all textures - check this
    val textureMaps: IndexedSeq[MeshSurfaceProperty[Point[_2D]]] = indirectTexture.properties.flatMap {
      case tex: TextureMappedProperty[RGBA] => Some(tex.textureMapping) // normal texture,
      case nontex: MeshSurfaceProperty[RGBA] => None // no texture, ignore
    }
    val firstMap = textureMaps.headOption
    val allEqual = textureMaps.forall(_ == firstMap.get)
    require(allEqual, "MSH multi texture save: texture maps are different")
    require(textureMaps.nonEmpty, "MSH multi texture save: no valid texture maps found")

    // extract texture coordinates for all points ... overwrite previous coordinates ... this only works if all are the same
    val texInTriangles: VertexPropertyPerTriangle[Point[_2D]] = VertexPropertyPerTriangle.fromSurfaceProperty(textureMaps.head)
    val tti: IndexedSeq[IntVector[_3D]] = texInTriangles.triangleVertexIndex
    val texCoordsData: Array[Point[_3D]] = texInTriangles.vertexData.toArray.map(p => Point(p.x, p.y, 0.0f))

    // indirect property must redirect to textures, remove all non textures, create MSHMaterial for each texture
    val textureMaterials: IndexedSeq[MSHMaterial] = for (p <- indirectTexture.properties.indices) yield indirectTexture.properties(p) match {
      case tex: TextureMappedProperty[RGBA] => createTextureMaterial(tex, p) // normal texture,
      case nontex: MeshSurfaceProperty[RGBA] => MSHMaterial.defaultMaterial // no texture, ignore
    }

    // normals
    val mshNormals = normals.map(n => VertexPropertyPerTriangle.fromSurfaceProperty(n))
    val tni: IndexedSeq[IntVector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.triangleVertexIndex).getOrElse(IndexedSeq.empty[IntVector[_3D]])
    val normalData: Array[Vector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.vertexData.toArray).getOrElse(Array.empty[Vector[_3D]])

    MSHMesh(
      textureMaterials.toArray,
      mesh.pointSet.points.map(p => p: Point[_3D]).toArray,
      normalData,
      texCoordsData,
      Array.empty[RGBA],
      triangulation.triangles.map(_.toIntVector3D).toArray,
      tni.toArray,
      tti.toArray,
      Array.empty[IntVector[_3D]],
      indirectTexture.triangleIndirectionIndex.toArray,
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[Int],
      Array.empty[Int],
      new File(""))
  }

  def fromGravisMesh3DWithVertexColor(mesh: TriangleMesh3D, color: MeshSurfaceProperty[RGBA], normals: Option[MeshSurfaceProperty[Vector[_3D]]]): MSHMesh = {
    val triangulation = mesh.triangulation
    // extract color with our triangle indirection: magic handled in fromSurfaceProperty, chooses best representation based on actual type
    val colorPerTriangle = VertexPropertyPerTriangle.fromSurfaceProperty(color)

    // normals
    val mshNormals = normals.map(n => VertexPropertyPerTriangle.fromSurfaceProperty(n))
    val tni: IndexedSeq[IntVector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.triangleVertexIndex).getOrElse(IndexedSeq.empty[IntVector[_3D]])
    val normalData: Array[Vector[_3D]] = mshNormals.map((n: VertexPropertyPerTriangle[Vector[_3D]]) => n.vertexData.toArray).getOrElse(Array.empty[Vector[_3D]])

    MSHMesh(
      Array(MSHMaterial.defaultMaterial),
      mesh.pointSet.points.map(p => p: Point[_3D]).toArray,
      normalData,
      Array.empty[Point[_3D]],
      colorPerTriangle.vertexData.toArray,
      triangulation.triangles.map(_.toIntVector3D).toArray,
      tni.toArray,
      Array.empty[IntVector[_3D]],
      colorPerTriangle.triangleVertexIndex.toArray,
      Array.fill(triangulation.triangleIds.size)(0),
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[IntVector[_2D]],
      Array.empty[Int],
      Array.empty[Int],
      new File(""))
  }
}

/** texture of an MSH mesh */
case class MSHTexture(image: PixelImage[RGBA], file: File)

object MSHTexture {
  def fromFile(file: File) = MSHTexture(PixelImageIO.read[RGBA](file).get, file)
}

/** material of an MSH mesh */
case class MSHMaterial(name: String,
                       ambient: RGBA,
                       diffuse: RGBA,
                       specular: RGBA,
                       shininess: Double,
                       texture: Option[MSHTexture])

object MSHMaterial {
  val defaultMaterial = new MSHMaterial("defaultMaterial", RGBA.White, RGBA.White, RGBA.White * 0.12f, 20f, None)

  def defaultMaterialWithTexture(texture: MSHTexture) = new MSHMaterial("defaultMaterialWithTexture", RGBA.White, RGBA.White, RGBA.White * 0.12f, 20f, Some(texture))
}
