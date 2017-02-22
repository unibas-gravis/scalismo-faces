package scalismo.faces.color

import java.awt.Color

import scalismo.faces.FacesTestSuite

class SRGBTests extends FacesTestSuite {

  describe("sRGB") {
    it("is within tolerance for round trips on the inverval [0,1]") {
      def roundTripCheck() = {
        val color = randomSRGB
        val trip = color.toRGB.toSRGB
        val tol = 1e-14
        math.abs(trip.r - color.r) < tol && math.abs(trip.r - color.r) < tol && math.abs(trip.r - color.r) < tol
      }
      (0.0 to 1.0 by 1.0/1000.0).forall(el => roundTripCheck())
    }

    it("negative values not allowed") {
      assertThrows[IllegalArgumentException]{
        SRGB(-1.0,-0.3,-0.1)
      }
    }

    it("can be converted to Java AWT Color") {
      val awtColor = Color.CYAN
      SRGB(0, 1, 1).toAWTColor shouldBe awtColor
    }

    it("can be created from a Java AWT Color") {
      val awtColor = Color.CYAN
      SRGB(awtColor) shouldBe SRGB(0, 1, 1)
    }

    it("AWT color can be converted to sRGB and back") {
      val color = randomSRGB.toAWTColor
      SRGB(color).toAWTColor shouldBe color
    }
  }
}
