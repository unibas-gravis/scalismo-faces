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

    it("reads well") {
      //val image = PixelImageIO.read[sRGB](new File("/export/faces/model/model2012.2/reference/mean2012-bfm_0.png")).get
      val verlauf = randomImage(256,256)//PixelImage(256,256, (x,y)=> RGB( x/255.0 ))
      PixelImageIO.write[RGB](verlauf, new File("/tmp/00000_random.png")).get
      val read = PixelImageIO.read[RGB](new File("/tmp/00000_random.png")).get
      println(read(127,127), verlauf(127,127))
      import PixelImage.implicits._
      println(math.sqrt((verlauf-read).normSq/(256*256)))

      PixelImageIO.write[RGB](read, new File("/tmp/00000_random_read.png")).get
      false shouldBe true
    }

  }

}
