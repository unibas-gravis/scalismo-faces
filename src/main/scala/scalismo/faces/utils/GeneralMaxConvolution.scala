package scalismo.faces.utils

import scalismo.faces.image.PixelImage
import scalismo.geometry.{Point, Point1D, _1D}
import scalismo.sampling.DistributionEvaluator

/** perform general max/min convolutions, such as distance transforms etc. */
object GeneralMaxConvolution {

  /** calculate a separable max convolution efficiently */
  def separableMaxConvolution(pixelImage: PixelImage[Double], eval: DistributionEvaluator[Point[_1D]]): PixelImage[Double] = {
    val tmp = pixelImage.toBuffer

    for (col <- 0 until tmp.domain.height) {
      val row = pixelImage.row(col)

      val tRow = full1DMaxConvolution(row.toArray, eval)
      for (x <- 0 until tmp.domain.width) {
        tmp(x, col) = tRow(x)
      }
    }

    val tImg = tmp.toImage

    for (row <- 0 until tmp.domain.width) {
      val col = tImg.col(row)
      val tCol = full1DMaxConvolution(col.toArray, eval)
      for (y <- 0 until tmp.domain.height) {
        tmp(row, y) = tCol(y)
      }
    }

    tmp.toImage
  }

  /** calculate 1D convolution */
  def full1DMaxConvolution(data: Array[Double], eval: DistributionEvaluator[Point[_1D]]): Array[Double] = {

    var maximumsPosition = 0
    var tmp = data.clone().toBuffer

    // keep this line if the local position is often the best one
    tmp.transform(t => t + eval.logValue(Point1D(0f)))

    // forward pass
    for (i <- data.indices) {
      var mPos = i
      for (lag <- maximumsPosition until i) {
        val t = data(lag) + eval.logValue(Point1D(i - lag))
        if (tmp(i) < t) {
          tmp(i) = t
          mPos = lag
        }
      }
      maximumsPosition = mPos
    }

    // backward pass
    for (i <- maximumsPosition to 0 by -1) {
      var mPos = i
      for (lag <- maximumsPosition until i by -1) {
        val t = data(lag) + eval.logValue(Point1D(i - lag))
        if (tmp(i) < t) {
          tmp(i) = t
          mPos = lag
        }
      }
      maximumsPosition = mPos
    }
    tmp.toArray
  }
}
