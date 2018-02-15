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

package scalismo.faces.texture


import scalismo.common.PointId
import scalismo.faces.image.PixelImageDomain
import scalismo.faces.mesh.DiscreteLaplaceBeltrami.LaplaceBeltramiWeightingFunction
import scalismo.faces.mesh.{DiscreteLaplaceBeltrami, TextureMappedProperty}
import scalismo.faces.numerics.ArnoldiSymmetricEigenSolver.EigenvaluesFirst
import scalismo.faces.numerics.MultiDimensionalScaling.MDSWeightFunction
import scalismo.faces.numerics.{ArnoldiSymmetricEigenSolver, MultiDimensionalScaling}
import scalismo.geometry.{Point, _2D, _3D}
import scalismo.mesh.{SurfacePointProperty, TriangleList, TriangleMesh3D}


object MeshParametrization {

  object MDSTextureMapping {
    /** Computes a texture mapping for a given mesh. Tries to preserve the euclidean distances between the mesh vertices in uv space. */
    def apply(mesh: TriangleMesh3D, textureDomain: PixelImageDomain): SurfacePointProperty[Point[_2D]] = {
      def vertexDistance(a: PointId, b: PointId) = (mesh.position.atPoint(a)- mesh.position.atPoint(b)).norm
      val embedding3d = MultiDimensionalScaling.parametrizationAsProperty(mesh.triangulation, vertexDistance)
      normalizeUV(embedding3d.mapPoints(p=>Point(p.x, p.y)), textureDomain, mesh.triangulation)
    }

    /** Computes a texture mapping for a given mesh. Tries to preserve the supplied distances in uv space. */
    def apply(triangulation: TriangleList, vertexDistance: MDSWeightFunction, textureDomain: PixelImageDomain): SurfacePointProperty[Point[_2D]] = {
      val embedding3d = MultiDimensionalScaling.parametrizationAsProperty(triangulation, vertexDistance)
      normalizeUV(embedding3d.mapPoints(p=>Point(p.x, p.y)), textureDomain, triangulation)
    }
  }

  object LaplacianEigenmapTextureMapping {
    /** Computes a laplacian eigenmap texture mapping for a given mesh. */
    def apply(mesh: TriangleMesh3D, textureDomain: PixelImageDomain): SurfacePointProperty[Point[_2D]] = {
      val embedding3d = laplacianEigenmap(mesh.triangulation, DiscreteLaplaceBeltrami.cotangentWeight(mesh))
      normalizeUV(embedding3d.mapPoints(p=>Point(p.x, p.y)), textureDomain, mesh.triangulation)
    }

    /** Calculates a new mesh parametrisation for a given mesh and a LaplaceBeltramiWeightingFunction.
      *
      * @param triangulation
      * @param vertexWeightFun only needs values for adjacent points.
      * @param maxIterFactor Increase from default setting of 3 if not enough.
      *                      Maximum amount of iterations for finding the eigenvectors
      *                      is numberOfPoints*maxIterFactor.
      * @return SurfacePointProperty[Point_3D], the parametrisation
      */
    def laplacianEigenmap(triangulation: TriangleList, vertexWeightFun: LaplaceBeltramiWeightingFunction, maxIterFactor: Int = 3): SurfacePointProperty[Point[_3D]] = {
      val C = DiscreteLaplaceBeltrami.laplaceBeltramiMatrix(triangulation, vertexWeightFun)
      val maxIter = triangulation.pointIds.length * maxIterFactor // this value seems to depend on the problem. Increase maxIterFactor if necessary.
      val tol = 1e-10
      val (eigenValues, eigenvectors) = ArnoldiSymmetricEigenSolver.symmetricEigs(v => C * v, C.cols, 4, EigenvaluesFirst.SmallestMagnitude, tol, maxIter)
      val embedding = IndexedSeq(eigenvectors(::, 1), //the first eigenvector is not used. It is the offset or "ambient" component.
        eigenvectors(::, 2),
        eigenvectors(::, 3))
      val embeddedValues = (0 until C.rows).map(i => Point(embedding(0)(i).toFloat, embedding(1)(i), embedding(2)(i)))
      SurfacePointProperty(triangulation, embeddedValues)
    }
  }

  /** TextureMapping: Normalizes the x and y coordinates of the parametrisation to the u,v coordinate range.
    *
    * Can be used to create a TextureMappedProperty.
    *
    * @param parametrisation
    * @param parametrisationDomain
    * @param triangulation
    * @return
    */
  def normalizeUV(parametrisation: SurfacePointProperty[Point[_2D]], parametrisationDomain: PixelImageDomain, triangulation: TriangleList): SurfacePointProperty[Point[_2D]] = {

    def normalizeParametrisationToUV(embeddedValues: IndexedSeq[Point[_2D]], textureSize: PixelImageDomain): IndexedSeq[Point[_2D]] = {
      //normalization of coordinates to texture image size [0, w] x [0, h]
      def getMinMax(values: IndexedSeq[Point[_2D]]) = {
        val lst = values.map(_.x)
        val maxx = lst.max
        val minx = lst.min
        val lstY = values.map(_.y)
        val maxy = lstY.max
        val miny = lstY.min
        (maxx, minx, maxy, miny)
      }
      val (maxx, minx, maxy, miny) = getMinMax(embeddedValues)
      val max = math.max(maxx, maxy)
      val min = math.min(minx, miny)
      val w = textureSize.width
      val h = textureSize.height
      embeddedValues.map(p => Point((p.x - min) / (max - min) * w, (p.y - min) / (max - min) * h))
    }

    val embeddedValues: IndexedSeq[Point[_2D]] = parametrisation.pointData.map(p => Point(p.x, p.y))
    val normalizedEmbedding = normalizeParametrisationToUV(embeddedValues, parametrisationDomain)
    val w = parametrisationDomain.width
    val h = parametrisationDomain.height
    SurfacePointProperty(triangulation, normalizedEmbedding.map(p => TextureMappedProperty.imageCoordinatesToUV(p, w, h)))
  }
}
