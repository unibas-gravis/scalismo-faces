package scalismo.faces.color

import java.awt.Color

import scalismo.faces.FacesTestSuite

class sRGBTests extends FacesTestSuite {

  describe("sRGB") {
    it("is within tolerance for round trips on the inverval [0,1]") {
      def roundTripCheck() = {
        val color = randomsRGB
        val trip = color.toRGB.tosRGB
        val tol = 1e-14
        math.abs(trip.r - color.r) < tol && math.abs(trip.r - color.r) < tol && math.abs(trip.r - color.r) < tol
      }
      (0.0 to 1.0 by 1.0/1000.0).map(el => roundTripCheck()).forall( el => el == true)
    }

    it("negative values not allowed") {
      assertThrows[IllegalArgumentException]{
        sRGB(-1.0,-0.3,-0.1)
      }
    }

    it("can be converted to Java AWT Color") {
      val awtColor = Color.CYAN
      sRGB(0, 1, 1).toAWTColor shouldBe awtColor
    }

    it("can be created from a Java AWT Color") {
      val awtColor = Color.CYAN
      sRGB(awtColor) shouldBe sRGB(0, 1, 1)
    }

    it("supports a roundtrip conversion to/from AWT Color") {
      val color = randomsRGB
      sRGB(color.toAWTColor).toAWTColor shouldBe color.toAWTColor
    }
  }
}
