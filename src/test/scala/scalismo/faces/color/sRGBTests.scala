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
      (0.0 to 1.0 by 1.0/1000.0).forall(el => roundTripCheck())
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

    it("AWT color can be converted to sRGB and back") {
      val color = randomsRGB.toAWTColor
      sRGB(color).toAWTColor shouldBe color
    }
  }
}
