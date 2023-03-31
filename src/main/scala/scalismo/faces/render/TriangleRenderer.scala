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

package scalismo.faces.render

import scalismo.faces.image._
import scalismo.geometry._
import scalismo.mesh._

import scala.reflect.ClassTag

/** main renderer object to render images of triangle meshes */
object TriangleRenderer {
  @inline
  private def vs2D(p: Point[_3D]): Point[_2D] = Point(p.x, p.y)

  /**
   * rasterize a single triangle
   *
   * @param triangleId
   *   triangle id in triangulation
   * @param a
   *   point A in triangle
   * @param b
   *   point B in triangle
   * @param c
   *   point C in triangle
   * @param pointShader
   *   performs correction of BCC from screen space to eye space
   * @param pixelShader
   *   called to paint each pixel of the triangle
   * @param buffer
   *   render buffer, stores the painted values and does depth management
   */
  def rasterTriangle[A](triangleId: TriangleId,
                        a: Point[_3D],
                        b: Point[_3D],
                        c: Point[_3D],
                        pointShader: PointShader,
                        pixelShader: PixelShader[A],
                        buffer: RenderBuffer[A]
  ): Unit = {
    // bounding box cropped to viewport size
    val minX = math.min(math.max(math.min(a.x, math.min(b.x, c.x)).floor.toInt, 0), buffer.width - 1)
    val maxX = math.min(math.max(math.max(a.x, math.max(b.x, c.x)).ceil.toInt, 0), buffer.width - 1)
    val minY = math.min(math.max(math.min(a.y, math.min(b.y, c.y)).floor.toInt, 0), buffer.height - 1)
    val maxY = math.min(math.max(math.max(a.y, math.max(b.y, c.y)).ceil.toInt, 0), buffer.height - 1)

    // determine winding order
    val signedArea = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
    val ccwWinding = signedArea > 0

    // point inside? careful with ccw/cw windings, shader decides what to do in each case
    @inline
    def inTriangle(x: Int, y: Int): Boolean = {
      val xf = x.toDouble
      val yf = y.toDouble
      // all "left of line"? or all "right of line"?
      if (ccwWinding)
        (b.x - a.x) * (yf - a.y) - (b.y - a.y) * (xf - a.x) >= 0 &&
        (c.x - b.x) * (yf - b.y) - (c.y - b.y) * (xf - b.x) >= 0 &&
        (a.x - c.x) * (yf - c.y) - (a.y - c.y) * (xf - c.x) >= 0
      else
        (b.x - a.x) * (yf - a.y) - (b.y - a.y) * (xf - a.x) <= 0 &&
        (c.x - b.x) * (yf - b.y) - (c.y - b.y) * (xf - b.x) <= 0 &&
        (a.x - c.x) * (yf - c.y) - (a.y - c.y) * (xf - c.x) <= 0
    }

    // reduce z interpolation to two multiplications only, using z differences
    val z10 = b.z - a.z
    val z20 = c.z - a.z

    // inner loop over pixels in bounding box
    var y = minY
    while (y <= maxY) {
      var x = minX
      while (x <= maxX) {
        // viewport check is not needed here anymore
        if (inTriangle(x, y)) {
          val screenBCC = BarycentricCoordinates.pointInTriangle(Point(x, y), vs2D(a), vs2D(b), vs2D(c))
          val worldBCC = pointShader.bccScreenToWorld(screenBCC, a, b, c)
          val z: Double = a.z + z10 * worldBCC.b + z20 * worldBCC.c
          buffer(x, y, z) = pixelShader(triangleId, worldBCC, Point(x, y, z), ccwWinding)
        }
        x += 1
      }
      y += 1
    }
  }

  /**
   * render a mesh, renders each triangle into the rendering buffer
   *
   * @param mesh
   *   mesh to render, a collection of triangles
   * @param pointShader
   *   called to transform points from object space to NDC in the canonical viewing volume
   * @param pixelShader
   *   called to paint the pixels of the rendered image
   * @param buffer
   *   holds the rendered values, does z management
   */
  def renderMesh[A](mesh: TriangleMesh[_3D],
                    pointShader: PointShader,
                    pixelShader: PixelShader[A],
                    buffer: RenderBuffer[A]
  ): RenderBuffer[A] = {
    // viewport transform
    val screenTransform = WindowTransform(buffer.width, buffer.height)

    renderMesh(mesh, pointShader, screenTransform, pixelShader, buffer)
  }

  /**
   * render a mesh, renders each triangle into the rendering buffer. This function uses a user-defined screenTransform.
   * NOTE: Use this function only when you know what you are doing.
   *
   * @param mesh
   *   mesh to render, a collection of triangles
   * @param pointShader
   *   called to transform points from object space to NDC in the canonical viewing volume
   * @param pixelShader
   *   called to paint the pixels of the rendered image
   * @param buffer
   *   holds the rendered values, does z management
   */
  def renderMesh[A](mesh: TriangleMesh[_3D],
                    pointShader: PointShader,
                    screenTransform: InvertibleTransform3D,
                    pixelShader: PixelShader[A],
                    buffer: RenderBuffer[A]
  ): RenderBuffer[A] = {

    // render all points
    val points = new Array[Point[_3D]](mesh.pointSet.numberOfPoints)
    mesh.pointSet.pointIds.foreach { pId =>
      val pointOnPlane = pointShader(mesh.pointSet.point(pId))
      points(pId.id) = screenTransform(pointOnPlane)
    }
    // render all fragments: painting/shading
    mesh.triangulation.triangleIds.foreach { tid =>
      val t = mesh.triangulation.triangle(tid)
      val a = points(t.ptId1.id)
      val b = points(t.ptId2.id)
      val c = points(t.ptId3.id)
      rasterTriangle(tid, a, b, c, pointShader, pixelShader, buffer)
    }
    buffer // return reference to buffer: works with anonymous buffers
  }

  /**
   * render a mesh, renders each triangle which passes the filter into the rendering buffer. This function uses a
   * user-defined screenTransform and a triangle filter to select triangles. NOTE: Use this function only when you know
   * what you are doing.
   *
   * @param mesh
   *   mesh to render, a collection of triangles
   * @param triangleFilter
   *   filter triangles to render, use for culling and clipping
   * @param pointShader
   *   called to transform points from object space to NDC in the canonical viewing volume
   * @param pixelShader
   *   called to paint the pixels of the rendered image
   * @param buffer
   *   holds the rendered values, does z management
   */
  def renderMesh[A](mesh: TriangleMesh[_3D],
                    triangleFilter: (TriangleId) => Boolean,
                    pointShader: PointShader,
                    screenTransform: InvertibleTransform3D,
                    pixelShader: PixelShader[A],
                    buffer: RenderBuffer[A]
  ): RenderBuffer[A] = {

    // render all points
    val points = new Array[Point[_3D]](mesh.pointSet.numberOfPoints)
    mesh.pointSet.pointIds.foreach { pId =>
      val pointOnPlane = pointShader(mesh.pointSet.point(pId))
      points(pId.id) = screenTransform(pointOnPlane)
    }
    // render all fragments: painting/shading
    mesh.triangulation.triangleIds.filter(triangleFilter).foreach { tid =>
      val t = mesh.triangulation.triangle(tid)
      val a = points(t.ptId1.id)
      val b = points(t.ptId2.id)
      val c = points(t.ptId3.id)
      rasterTriangle(tid, a, b, c, pointShader, pixelShader, buffer)
    }
    buffer // return reference to buffer: works with anonymous buffers
  }

  /**
   * render an arbitrary mesh surface property
   *
   * @param mesh
   *   mesh on which the property is defined
   * @param pointShader
   *   called to transform points from object space to NDC in the canonical viewing volume
   * @param property
   *   the surface property to render
   * @param buffer
   *   holds the rendered values, does z management
   */
  def renderProperty[A: ClassTag](mesh: TriangleMesh[_3D],
                                  pointShader: PointShader,
                                  property: MeshSurfaceProperty[A],
                                  buffer: RenderBuffer[Option[A]]
  ): RenderBuffer[Option[A]] = {
    renderMesh(
      mesh,
      pointShader,
      new PixelShader[Option[A]] {
        override def apply(triangleId: TriangleId, bcc: BarycentricCoordinates, screenCoordinates: Point[_3D]) =
          Some(property(triangleId, bcc))
      },
      buffer
    )
  }

  /** a triangle fragment, with information about triangle origin and the position within */
  case class TriangleFragment(mesh: TriangleMesh[_3D],
                              triangleId: TriangleId,
                              worldBCC: BarycentricCoordinates,
                              x: Int,
                              y: Int,
                              z: Double,
                              ccwWinding: Boolean
  )

  /** render a correspondence image into a buffer, contains information about triangle rasterization */
  def renderCorrespondence(mesh: TriangleMesh[_3D],
                           triangleFilter: (TriangleId) => Boolean,
                           pointShader: PointShader,
                           screenTransform: InvertibleTransform3D,
                           buffer: RenderBuffer[Option[TriangleFragment]]
  ): RenderBuffer[Option[TriangleFragment]] = {
    renderMesh[Option[TriangleFragment]](
      mesh,
      triangleFilter,
      pointShader,
      screenTransform,
      PixelShaders.CorrespondenceShader(mesh),
      buffer
    )
  }

  /** render a correspondence image into a buffer, contains information about triangle rasterization */
  def renderCorrespondence(mesh: TriangleMesh[_3D],
                           pointShader: PointShader,
                           buffer: RenderBuffer[Option[TriangleFragment]]
  ): RenderBuffer[Option[TriangleFragment]] = {
    renderMesh[Option[TriangleFragment]](
      mesh,
      pointShader,
      PixelShaders.CorrespondenceShader(mesh),
      buffer
    )
  }

  /** render window depth values (as used by Z buffer, in range [0, 1]) */
  def renderDepthWindow(mesh: TriangleMesh[_3D],
                        pointShader: PointShader,
                        buffer: RenderBuffer[Double]
  ): RenderBuffer[Double] = {
    val screen = WindowTransform(buffer.width, buffer.height)
    renderMesh(
      mesh,
      pointShader,
      new PixelShader[Double] {
        override def apply(triangleId: TriangleId,
                           worldBCC: BarycentricCoordinates,
                           screenCoordinates: Point[_3D]
        ): Double = screen(pointShader(mesh.position(triangleId, worldBCC))).z
      },
      buffer
    )
  }

  /**
   * Creates a Surface Property which tells on every point on the mesh surface its visibility according to the
   * pointShader. Procedure:
   *   1. Render surface coordinates with pointShader. 2. Because we want visibility of coordinates between two pixels,
   *      we interpolate between the pixels. 3. Compare if the z component of the mesh surface point is smaller than the
   *      z component of the corresponding rendered point. pt.z <= renderedPoint.z + offset An offset > 0 is necessary.
   *
   * boundaryAlwaysVisible: Problem: At mesh boundaries we interpolate the z value with the buffer bg value. Case 1,
   * bgValue is NegativeInfinity: boundaryAlwaysVisible = false Interpolation yields always NegativeInfinity. Thus, the
   * boundary pixels are always invisible. Case 2, bgValue is PositiveInfinity: Interpolation yields always
   * PositiveInfinity, thus the boundary pixels are always visible.
   *
   * @param mesh
   *   mesh to render
   * @param pointShader
   *   called to to transform object space points to NDC in the cononical view volume
   * @param pixelImageDomain
   *   domain of buffer to evaluate visibility (sampled with a z buffer)
   * @param offset
   *   numerical z buffer value offset ~1e-3
   * @param boundaryAlwaysVisible
   *   make boundary pixels (at occluding contours vs background) always visible
   */
  def visibilityAsSurfaceProperty(mesh: TriangleMesh[_3D],
                                  pointShader: PointShader,
                                  pixelImageDomain: PixelImageDomain,
                                  offset: Double,
                                  boundaryAlwaysVisible: Boolean
  ): MeshSurfaceProperty[Boolean] = {
    val zBufferImageDisc = renderDepthWindow(
      mesh,
      pointShader,
      ZBuffer[Double](pixelImageDomain.width,
                      pixelImageDomain.height,
                      if (boundaryAlwaysVisible) Double.PositiveInfinity else Double.NegativeInfinity
      )
    ).toImage
    val zBufferImage = zBufferImageDisc.interpolate
    new MeshSurfaceProperty[Boolean] {
      override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): Boolean = {
        val currentWorld = mesh.position(triangleId, bcc)
        val currentVP = transformPoint(currentWorld, pointShader, pixelImageDomain)
        val zBufferValue = zBufferImage(currentVP.x + 0.5, currentVP.y + 0.5)
        currentVP.z <= zBufferValue + offset // these values are handpicked.
      }

      override def triangulation: TriangleList = mesh.triangulation
    }
  }

  /** render an arbitrary mesh surface property as an image */
  def renderPropertyImage[A: ClassTag](mesh: TriangleMesh[_3D],
                                       pointShader: PointShader,
                                       property: MeshSurfaceProperty[A],
                                       width: Int,
                                       height: Int
  ): PixelImage[Option[A]] = {
    renderProperty(mesh, pointShader, property, ZBuffer[Option[A]](width, height, None)).toImage
  }

  /**
   * render a correspondence image for a given mesh into a new buffer, contains information about triangle rasterization
   */
  def renderCorrespondenceImage(mesh: TriangleMesh[_3D],
                                pointShader: PointShader,
                                width: Int,
                                height: Int
  ): PixelImage[Option[TriangleFragment]] = {
    renderCorrespondence(mesh, pointShader, ZBuffer[Option[TriangleFragment]](width, height, None)).toImage
  }

  /** render a point position in the given buffer domain using a point shader */
  def transformPoint(point: Point[_3D], pointShader: PointShader, bufferDomain: PixelImageDomain): Point[_3D] = {
    val screenTrafo = WindowTransform(bufferDomain.width, bufferDomain.height)
    screenTrafo(pointShader(point))
  }

  /** render a point position in the given buffer domain using a point shader */
  def transformPoint(point: Point[_3D], pointShader: PointShader, screenTrafo: Transform3D): Point[_3D] = {
    screenTrafo(pointShader(point))
  }
}
