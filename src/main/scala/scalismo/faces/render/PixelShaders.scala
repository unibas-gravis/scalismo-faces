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

import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.numerics.SphericalHarmonics
import scalismo.faces.parameters.SphericalHarmonicsLight
import scalismo.faces.render.TriangleRenderer.TriangleFragment
import scalismo.geometry._
import scalismo.mesh.{BarycentricCoordinates, MeshSurfaceProperty, TriangleId, TriangleMesh}

/** collection of common pixel shaders */
object PixelShaders {

  /** "shade" with a property value, renders property value directly into image */
  case class PropertyShader[A](property: MeshSurfaceProperty[A]) extends PixelShader[A] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): A = {
      property(triangleId, worldBCC)
    }
  }

  /** Lambertian reflectance shader, with additional ambient term */
  case class LambertShader(albedo: MeshSurfaceProperty[RGBA],
                           ambientLight: RGB,
                           diffuseLight: RGB,
                           lightDirection: Vector[_3D],
                           normals: MeshSurfaceProperty[Vector[_3D]]) extends PixelShader[RGBA] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): RGBA = {
      val c = albedo(triangleId, worldBCC)
      val n = normals(triangleId, worldBCC).normalize
      val diffuseFactor: Double = math.max(n.dot(lightDirection.normalize), 0.0)
      val diffuse: RGB = diffuseLight * diffuseFactor
      val combined = ambientLight + diffuse
      RGBA(c.r * combined.r, c.g * combined.g, c.b * combined.b, c.a) // radiance
    }

    def invert(reflectance: MeshSurfaceProperty[RGBA]) = InverseDiffuseShader(reflectance, ambientLight, diffuseLight, lightDirection, normals)
  }

  object LambertShader {
    /** Lambertian BRDF, constant value */
    case class LambertBRDF(albedo: RGB) extends BRDF[RGB] {
      override def apply(lightDirection: Vector[_3D], viewDirection: Vector[_3D]): RGB = albedo
    }
  }

  /** shader to invert diffuse lighting for de-illumination */
  case class InverseDiffuseShader(reflectance: MeshSurfaceProperty[RGBA],
                                  ambientLight: RGB,
                                  diffuseLight: RGB,
                                  lightDirection: Vector[_3D],
                                  normals: MeshSurfaceProperty[Vector[_3D]]) extends PixelShader[RGBA] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): RGBA = {
      val r = reflectance(triangleId, worldBCC)
      val n = normals(triangleId, worldBCC).normalize
      val diffuseFactor: Double = math.max(n.dot(lightDirection.normalize), 0.0)
      val diffuse: RGB = diffuseLight * diffuseFactor
      val combined = ambientLight + diffuse
      RGBA(r.r / combined.r, r.g / combined.g, r.b / combined.b, r.a)
    }
  }

  /**
    * Blinn-Phong specular shader
    *
    * @param positions mesh point positions in world space
    * @param normals   normals in world space
    */
  case class BlinnPhongSpecularShader(specularLight: RGB,
                                      lightDirection: Vector[_3D],
                                      positions: MeshSurfaceProperty[Point[_3D]],
                                      normals: MeshSurfaceProperty[Vector[_3D]],
                                      eyePosition: Point[_3D],
                                      shininess: Double) extends PixelShader[RGBA] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): RGBA = {
      val v = (eyePosition - positions(triangleId, worldBCC)).normalize
      val spec = BlinnPhongSpecularShader.brdf(shininess, normals(triangleId, worldBCC), lightDirection, v)
      (specularLight * spec).toRGBA
    }
  }

  object BlinnPhongSpecularShader {

    case class BlinnPhongBRDF(shininess: Double, normal: Vector[_3D]) extends BRDF[Double] {
      override def apply(lightDirection: Vector[_3D], viewDirection: Vector[_3D]): Double = {
        BlinnPhongSpecularShader.brdf(shininess, normal, lightDirection, viewDirection)
      }
    }

    def brdf(shininess: Double, normal: Vector[_3D], lightDirection: Vector[_3D], viewDirection: Vector[_3D]): Double = {
      val h = (viewDirection + lightDirection).normalize
      val cos = normal.normalize.dot(h)
      math.pow(math.max(cos, 0.0), shininess)
    }
  }

  /** Lambertian shader with efficient Spherical Harmonics environment map representation */
  case class SphericalHarmonicsLambertShader(albedo: MeshSurfaceProperty[RGBA],
                                             environmentMap: IndexedSeq[Vector[_3D]],
                                             normals: MeshSurfaceProperty[Vector[_3D]]) extends PixelShader[RGBA] {
    override def apply(triangleId: TriangleId,
                       worldBCC: BarycentricCoordinates,
                       screenCoordinates: Point[_3D]): RGBA = SphericalHarmonicsLambertShader.shade(
      albedo(triangleId, worldBCC),
      normals(triangleId, worldBCC),
      environmentMap)
  }

  object SphericalHarmonicsLambertShader {
    def shade(c: RGBA, n: Vector[_3D], environmentMap: IndexedSeq[Vector[_3D]]): RGBA = {
      val numCoeffs = math.min(environmentMap.size, 9)
      var radiance = Vector3D.zero

      var r = 0.0
      var g = 0.0
      var b = 0.0
      var i = 0
      while (i < numCoeffs) {
        val ksh = SphericalHarmonics.shBasisFunctionDirect(i, n) * SphericalHarmonicsLight.lambertKernel(i)
        val l = environmentMap(i)
        r += ksh * l.x
        g += ksh * l.y
        b += ksh * l.z
        i += 1
      }
      RGBA(r * c.r, g * c.g, b * c.b, c.a)
    }
  }

  /**
    * Phong illumination using Spherical Harmonics as Environment map.
    * Instead of using directional light we can use an environment map for lighting in the phong illumination model.
    * The specular part consists of: I_spec = I_in * specularFactor
    * We use this specular factor (phong):
    *    specularFactor = ( view dot reflected(view, normal) ) ^ shininess
    * But we take the light intensity from the SH environment map.
    *    I_l = SH(reflected(view, normal))
    * Because the environment map is smooth this approximation should not be too bad.
    */
  case class SphericalHarmonicsSpecularShader(specularExp: Double,
                                              environmentMap: IndexedSeq[Vector[_3D]],
                                              normalsWorld: MeshSurfaceProperty[Vector[_3D]],
                                              positionsWorld: MeshSurfaceProperty[Point[_3D]]) extends PixelShader[RGBA] {
    override def apply(triangleId: TriangleId,
                       worldBCC: BarycentricCoordinates,
                       screenCoordinates: Point[_3D]): RGBA = {
      SphericalHarmonicsSpecularShader.specularPart(positionsWorld(triangleId, worldBCC), normalsWorld(triangleId, worldBCC), specularExp, environmentMap)
    }
  }

  object SphericalHarmonicsSpecularShader {
    def specularPart(posWorld: Point[_3D], normalWorld: Vector[_3D], specularExp: Double, environmentMap: IndexedSeq[Vector[_3D]]): RGBA = {
      /** BRDF: find outgoing vector for given view vector (reflect) */
      val vecToEye: Vector3D = (Point3D(0, 0, 0) - posWorld).normalize
      val viewAdjusted = vecToEye * vecToEye.dot(normalWorld)
      val reflected = normalWorld + (normalWorld - viewAdjusted)
      /** Read out value in environment map in out direction for specularity */
      val lightInOutDirection = SphericalHarmonicsLambertShader.shade(RGBA(1.0, 1.0, 1.0), reflected, environmentMap)
      val specularFactor = math.pow(vecToEye.dot(reflected).toDouble, specularExp)
      specularFactor *: lightInOutDirection
    }
  }

  /** calculates triangle correspondence information per shaded pixel */
  case class CorrespondenceShader(mesh: TriangleMesh[_3D]) extends PixelShader[Option[TriangleFragment]] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): Some[TriangleFragment] = {
      Some(TriangleFragment(mesh, triangleId, worldBCC, screenCoordinates.x.toInt, screenCoordinates.y.toInt, screenCoordinates.z, ccwWinding = true))
    }

    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D], ccwWinding: Boolean): Some[TriangleFragment] = {
      Some(TriangleFragment(mesh, triangleId, worldBCC, screenCoordinates.x.toInt, screenCoordinates.y.toInt, screenCoordinates.z, ccwWinding))
    }
  }

  /** Oren-Nayar reflectance model for diffuse reflectance with micro-facets */
  case class OrenNayarShader(albedo: MeshSurfaceProperty[RGBA],
                             roughness: MeshSurfaceProperty[Double],
                             normals: MeshSurfaceProperty[Vector[_3D]],
                             position: MeshSurfaceProperty[Point[_3D]],
                             ambientLight: RGB,
                             diffuseLight: RGB,
                             lightDirection: Vector[_3D],
                             eyePosition: Point[_3D]) extends PixelShader[RGBA] {

    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): RGBA = {
      OrenNayarShader.shade(
        albedo(triangleId, worldBCC),
        roughness(triangleId, worldBCC),
        position(triangleId, worldBCC),
        ambientLight,
        diffuseLight,
        lightDirection,
        normals(triangleId, worldBCC),
        eyePosition
      )
    }
  }

  object OrenNayarShader {

    case class OrenNayarBRDF(normal: Vector[_3D], albedo: RGB, roughness: Double) extends BRDF[RGB] {
      override def apply(lightDirection: Vector[_3D], view: Vector[_3D]): RGB = {
        val r = OrenNayarShader.reflectanceFactor(roughness, normal, lightDirection, view)
        albedo * r
      }
    }

    def reflectanceFactor(roughness: Double, normal: Vector[_3D], lightDirection: Vector[_3D], view: Vector[_3D]): Double = {
      val A = 1.0 - 0.5 * (roughness / (roughness + 0.33))
      val B = 0.45 * (roughness / (roughness + 0.09))

      val cosPhiIn = normal.dot(lightDirection)
      val cosPhiOut = normal.dot(view)

      val cosAlpha = math.abs(math.min(cosPhiIn, cosPhiOut))
      val cosBeta = math.abs(math.max(cosPhiIn, cosPhiOut))

      val sinAlpha = math.sqrt(1.0 - cosAlpha * cosAlpha)
      val sinBeta = Math.sqrt(1.0 - cosBeta * cosBeta)

      val tanBeta = sinBeta / (cosBeta + 1e-5)

      val cosPhiInMinusPhiOut = cosAlpha * cosBeta + sinAlpha * sinBeta
      val G = A + (B * Math.max(0.0, cosPhiInMinusPhiOut) * sinAlpha * tanBeta)
      G
    }

    def shade(albedo: RGBA,
              roughness: Double,
              position: Point[_3D],
              lightAmbient: RGB,
              lightDiffuse: RGB,
              lightDirection: Vector[_3D],
              normal: Vector[_3D],
              eyePosition: Point[_3D]): RGBA = {

      val view = (eyePosition - position).normalize
      val reflectance = OrenNayarShader.reflectanceFactor(roughness, normal, lightDirection, view)
      val cosFactor = normal dot lightDirection

      val illum = lightDiffuse * (reflectance * math.max(0, cosFactor)) + lightAmbient
      RGBA(albedo.toRGB x illum, albedo.a)
    }
  }

  /** Cook-Torrance reflectance model for specular reflectance with micro-facets */
  case class CookTorranceSpecularShader(lightSpecular: RGB,
                                        frontalReflectance: MeshSurfaceProperty[Double],
                                        roughness: MeshSurfaceProperty[Double],
                                        normals: MeshSurfaceProperty[Vector[_3D]],
                                        positions: MeshSurfaceProperty[Point[_3D]],
                                        lightDirection: Vector[_3D],
                                        eyePosition: Point[_3D]) extends PixelShader[RGBA] {

    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): RGBA = {
      CookTorranceSpecularShader.shade(
        lightSpecular,
        frontalReflectance(triangleId, worldBCC),
        roughness(triangleId, worldBCC),
        normals(triangleId, worldBCC),
        positions(triangleId, worldBCC),
        lightDirection,
        eyePosition
      )
    }
  }

  object CookTorranceSpecularShader {

    case class CookTorranceBRDF(frontalReflectance: Double, roughness: Double, normal: Vector[_3D]) extends BRDF[Double] {
      override def apply(lightDirection: Vector[_3D], viewDirection: Vector[_3D]): Double = {
        CookTorranceSpecularShader.brdf(frontalReflectance, roughness, normal, lightDirection, viewDirection)
      }
    }

    def brdf(frontalReflectance: Double, roughness: Double, normal: Vector[_3D], lightDirection: Vector[_3D], viewDirection: Vector[_3D]): Double = {
      val eps = 1e-5
      val en = math.max(eps, viewDirection.dot(normal))
      val h: Vector[_3D] = {
        val d = lightDirection + viewDirection
        if (d.norm2 > eps * eps)
          d.normalize
        else
          normal
      }

      val D = ReflectanceHelper.beckmann(normal, h, roughness, 1e-4)
      val F = ReflectanceHelper.schlickFresnel(viewDirection, h, frontalReflectance)

      val hn2 = 2.0 * math.abs(h.dot(normal))
      val eh = math.max(eps, h.dot(viewDirection))
      val ln = math.abs(lightDirection.dot(normal))

      val G = math.min(1.0, math.min(hn2 * en / eh, hn2 * ln / eh))
      0.25 * D * F * G / en
    }

    def shade(lightSpecular: RGB,
              frontalReflectance: Double,
              roughness: Double,
              normal: Vector[_3D],
              position: Point[_3D],
              lightDirection: Vector[_3D],
              eyePosition: Point[_3D]): RGBA = {
      val view = (eyePosition - position).normalize
      val r = CookTorranceSpecularShader.brdf(frontalReflectance, roughness, normal, lightDirection, view)
      (lightSpecular * r).toRGBA
    }
  }

  /** paint pixels according to their triangle winding order, useful to debug your mesh normals */
  case class TriangleWindingShader[A](ccwColor: A, cwColor: A) extends PixelShader[A] {
    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D], ccwWinding: Boolean): A = if (ccwWinding) ccwColor else cwColor

    override def apply(triangleId: TriangleId, worldBCC: BarycentricCoordinates, screenCoordinates: Point[_3D]): A = ccwColor
  }
}

object ReflectanceHelper {
  // Note the removal of the 1/pi factor from the Beckmann distribution
  /** Beckmann distribution */
  def beckmann(n: Vector[_3D], h: Vector[_3D], m: Double, eps: Double): Double = {
    val cosAlpha = math.max(eps, n.dot(h))
    math.exp(-(1.0 - cosAlpha * cosAlpha) / (cosAlpha * cosAlpha * m * m)) / (m * m * cosAlpha * cosAlpha * cosAlpha * cosAlpha)
  }

  /** Schlick approximation to Fresnel terms */
  def schlickFresnel(e: Vector[_3D], h: Vector[_3D], r0: Double, lambda: Double): Double = {
    r0 + (1.0 - r0) * math.pow(1.0 - math.abs(e.dot(h)), 5.0)
  }

  /** Schlick approximation to Fresnel terms */
  def schlickFresnel(e: Vector[_3D], h: Vector[_3D], r0: Double): Double = {
    schlickFresnel(e, h, r0, 5.0)
  }
}
