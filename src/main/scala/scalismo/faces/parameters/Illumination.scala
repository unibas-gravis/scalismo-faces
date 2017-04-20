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

import breeze.linalg.DenseVector
import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.mesh.ColorNormalMesh3D
import scalismo.faces.numerics.SphericalHarmonics
import scalismo.faces.render.PixelShaders.SphericalHarmonicsLambertShader
import scalismo.faces.render.{PixelShader, PixelShaders}
import scalismo.geometry.{Point, Vector, Vector3D, _3D}

/** Illumination to shade meshes */
sealed trait Illumination {
  def shader(worldMesh: ColorNormalMesh3D, eyePosition: Point[_3D]): PixelShader[RGBA]
}

/** directional illumination with Phong reflectance */
case class DirectionalLight(ambient: RGB,
                            diffuse: RGB,
                            direction: Vector[_3D],
                            specular: RGB,
                            shininess: Double = 10.0) extends Illumination {

  override def shader(worldMesh: ColorNormalMesh3D, eyePosition: Point[_3D]): PixelShader[RGBA] = {
    val diffuseShader = PixelShaders.LambertShader(worldMesh.color, ambient, diffuse, direction.normalize, worldMesh.normals)
    val specularShader = PixelShaders.BlinnPhongSpecularShader(specular, direction, worldMesh.shape.position, worldMesh.normals, eyePosition, shininess)
    diffuseShader + specularShader
  }
}

object DirectionalLight {
  /** default ambient-only light, direct reproduction of color values, no shading */
  val ambient = DirectionalLight(RGB.White, RGB.Black, Vector3D.unitZ, RGB.Black, 10.0)

  val off = DirectionalLight(RGB.Black, RGB.Black, Vector3D.unitZ, RGB.Black, 10.0)
}

/** Spherical Harmonics illumination parameters, environment map */
case class SphericalHarmonicsLight(coefficients: IndexedSeq[Vector[_3D]]) extends Illumination {
  require(coefficients.isEmpty || SphericalHarmonics.totalCoefficients(bands) == coefficients.length, "invalid length of coefficients to build SphericalHarmonicsLight")

  val nonEmpty: Boolean = coefficients.nonEmpty

  override def shader(worldMesh: ColorNormalMesh3D, eyePosition: Point[_3D]): SphericalHarmonicsLambertShader = shader(worldMesh)

  /** SH shader for a given mesh and this environment map */
  def shader(worldMesh: ColorNormalMesh3D): SphericalHarmonicsLambertShader = {
    PixelShaders.SphericalHarmonicsLambertShader(worldMesh.color, coefficients, worldMesh.normals)
  }

  /** linearize coefficients to a Breeze vector (r, g, b, r, g, b ... ) */
  def toBreezeVector: DenseVector[Double] = DenseVector(coefficients.flatMap(v => IndexedSeq(v.x, v.y, v.z)).toArray)

  /** total coefficients energy */
  def energy: Double = coefficients.map(_.norm2).sum

  /** number of bands */
  def bands: Int = SphericalHarmonics.numberOfBandsForCoefficients(coefficients.size)

  /** change number of bands */
  def withNumberOfBands(numberOfBands: Int): SphericalHarmonicsLight = {
    if (numberOfBands < bands)
      SphericalHarmonicsLight(
        coefficients.take(SphericalHarmonics.totalCoefficients(numberOfBands))
      )
    else if (numberOfBands > bands)
      SphericalHarmonicsLight(
        coefficients ++
          IndexedSeq.fill(SphericalHarmonics.totalCoefficients(numberOfBands) - coefficients.length)(Vector3D.zero)
      )
    else
      this
  }
}

object SphericalHarmonicsLight {

  import scalismo.geometry.Vector._

  /** values of the Lambertian reflectance kernel expressed in SH */
  val lambertKernel = Array(
    3.141593,
    2.094395,
    2.094395,
    2.094395,
    0.785398,
    0.785398,
    0.785398,
    0.785398,
    0.785398
  )

  /** neutral ambient white light (renders pure albedo) */
  val ambientWhite = SphericalHarmonicsLight(IndexedSeq(Vector(
    1.0 / lambertKernel(0) / SphericalHarmonics.N0,
    1.0 / lambertKernel(0) / SphericalHarmonics.N0,
    1.0 / lambertKernel(0) / SphericalHarmonics.N0
  )))

  /** default frontal illumination */
  val frontal: SphericalHarmonicsLight = fromAmbientDiffuse(RGB(0.8), RGB(0.2), Vector3D.unitZ)

  /** construct from directional light source with diffuse reflectance */
  def fromAmbientDiffuse(ambient: RGB, diffuse: RGB, direction: Vector[_3D]): SphericalHarmonicsLight = {
    val n0 = SphericalHarmonics.N0
    val n1 = SphericalHarmonics.N1

    val dir: Vector[_3D] = direction.normalize
    val amb = Vector(ambient.r, ambient.g, ambient.b)
    val diff = Vector(diffuse.r, diffuse.g, diffuse.b)

    SphericalHarmonicsLight(IndexedSeq(
      (1f / n0 / lambertKernel(0)) *: amb,
      (dir.y / n1 / lambertKernel(1)) *: diff,
      (dir.z / n1 / lambertKernel(2)) *: diff,
      (dir.x / n1 / lambertKernel(3)) *: diff
    ))
  }

  def fromBreezeVector(dv: DenseVector[Double]): SphericalHarmonicsLight = {
    require(dv.length % 3 == 0, "length of array to build SHLight must be a multiple of 3")
    val grouped = dv.toArray.grouped(3).map(g => Vector[_3D](g)).toIndexedSeq
    SphericalHarmonicsLight(grouped)
  }

  /** Finds the principal light direction in the spherical harmonics with respect to light intensity.
    * lightIntensity(n) = \sum_{i=0} Y_i(n) * lambertKernel_i * ( shl_r(i) + shl_g(i) + shl_b(i) ) */
  def directionFromSHLightIntensity(shl: SphericalHarmonicsLight): Option[Vector[_3D]] = {
    if (shl.coefficients.length >= 4) {
      //Need shl with first band for a directional part
      val c1 = shl.coefficients(1).x + shl.coefficients(1).y + shl.coefficients(1).z
      val c2 = shl.coefficients(2).x + shl.coefficients(2).y + shl.coefficients(2).z
      val c3 = shl.coefficients(3).x + shl.coefficients(3).y + shl.coefficients(3).z
      val y = c1 * SphericalHarmonics.N1 / SphericalHarmonicsLight.lambertKernel(1)
      val z = c2 * SphericalHarmonics.N1 / SphericalHarmonicsLight.lambertKernel(2)
      val x = c3 * SphericalHarmonics.N1 / SphericalHarmonicsLight.lambertKernel(3)
      val v = Vector(x, y, z)
      val len = v.norm //directedness
      val eps = 1e-12
      if(len > eps)
        Some(v.normalize)
      else
        None
    }else{
      None
    }
  }

  /** number of coefficients in n bands */
  def coefficientsInBands(n: Int): Int = SphericalHarmonics.totalCoefficients(n)

  /** empty coefficients vector */
  val empty = SphericalHarmonicsLight(IndexedSeq.empty[Vector[_3D]])

  /** empty coefficients vector */
  def zero(numberOfBands: Int) = SphericalHarmonicsLight(IndexedSeq.fill(SphericalHarmonics.totalCoefficients(numberOfBands))(Vector3D.zero))
}
