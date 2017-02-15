package scalismo.faces.color

import java.io.File

import scalismo.faces.FacesTestSuite
import scalismo.faces.image.{PixelImage, PixelImageIO}

class sRGBTests extends FacesTestSuite {

  def randomsRGB = sRGB(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  describe("sRGB") {
    it("is within tolerance for random round trips") {
      def roundTripCheck() = {
        val color = randomsRGB
        val trip = color.toRGB.tosRGB
        val tol = 1.0 / 256.0 / 2.0 // half of 8 bit granularity
        math.abs(trip.r - color.r) < tol && math.abs(trip.r - color.r) < tol && math.abs(trip.r - color.r) < tol
      }
      (0 until 1000).map(el => roundTripCheck()).forall( el => el == true)
    }

    it("negative values not allowed") {
      assertThrows[IllegalArgumentException]{
        sRGB(-1.0,-0.3,-0.1)
      }
    }

  }

}
