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
import scalismo.faces.FacesTestSuite
import scalismo.color.RGB
import scalismo.geometry.{_3D, EuclideanVector, EuclideanVector3D}

class SphericalHarmonicsLightTests extends FacesTestSuite {

  describe("SphericalHarmonicsLight parameters") {
    it("can be constructed with specific number of bands") {
      val sh5 = SphericalHarmonicsLight.zero(5)
      sh5.bands shouldBe 5
      sh5.coefficients.length shouldBe SphericalHarmonicsLight.coefficientsInBands(5)
    }

    it("can be rescaled to more components") {
      val sh = SphericalHarmonicsLight.frontal
      assert(sh.bands == 1)

      val band2 = sh.withNumberOfBands(2)
      band2.coefficients.length shouldBe 9
      band2.bands shouldBe 2

      band2.coefficients.take(4) shouldBe sh.coefficients
      band2.coefficients.drop(4) shouldBe IndexedSeq.fill(5)(EuclideanVector3D.zero)
    }

    it("can be rescaled to fewer components") {
      val sh = SphericalHarmonicsLight.frontal
      assert(sh.bands == 1)

      val band0 = sh.withNumberOfBands(0)
      band0.coefficients.length shouldBe 1
      band0.bands shouldBe 0

      band0.coefficients(0) shouldBe sh.coefficients(0)
    }

    it("SH to DenseVector and from DenseVector") {
      val sh = SphericalHarmonicsLight.frontal
      val shB = sh.toBreezeVector
      val shN = SphericalHarmonicsLight.fromBreezeVector(shB)
      sh shouldBe shN

      val bV = DenseVector(Array.fill(27)(rnd.scalaRandom.nextGaussian()))
      val shL = SphericalHarmonicsLight.fromBreezeVector(bV)
      val nV = shL.toBreezeVector
      bV.toArray should contain theSameElementsInOrderAs nV.toArray
    }

    it("avoids building of invalid coefficients") {
      val wrongLength = IndexedSeq.fill(2)(EuclideanVector3D.zero)
      an[IllegalArgumentException] should be thrownBy SphericalHarmonicsLight(wrongLength)
    }

    describe("To recover a principal direction of illumination, SphericalHarmonicsLight") {
      it("extracts consistently the direction from randomly generated directed spherical harmonics.") {

        /**
         * Measures direction discrepancy between fromAmbientDiffuse and directionFromSH. Creates directed illuminations
         * for random directions with fromAmbientDiffuse and solves for the direction with directionFromSH.
         */
        def testDirectionFromSH(eps: Double, repeat: Int): Boolean = {
          def genDirLight(v: EuclideanVector[_3D]) = SphericalHarmonicsLight.fromAmbientDiffuse(
            RGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble()),
            RGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble()),
            v
          )
          def randDirection = EuclideanVector.fromSpherical(1.0,
                                                            rnd.scalaRandom.nextDouble() * math.Pi,
                                                            rnd.scalaRandom.nextDouble() * math.Pi * 2.0
          ) // non-uniform on the sphere! Does not matter for the test.
          (0 until repeat).forall { _ =>
            val v = randDirection.normalize
            val rec = SphericalHarmonicsLight.directionFromSHLightIntensity(genDirLight(v))
            if (rec.isDefined) { // if we find a direction
              val d = (v - rec.get).norm
              d < eps
            } else // if no direction is found. Should always find a direction in this test.
              false
          }
        }

        testDirectionFromSH(1e-14, 50) shouldBe true
      }

      it("gives sensible results for undirected SHL.") {
        val rec = SphericalHarmonicsLight.directionFromSHLightIntensity(SphericalHarmonicsLight.ambientWhite)
        rec.isDefined shouldBe false
      }
    }
  }
}
