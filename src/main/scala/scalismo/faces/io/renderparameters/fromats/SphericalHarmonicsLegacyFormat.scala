package scalismo.faces.io.renderparameters.fromats

import scalismo.geometry.{_3D, EuclideanVector}
import scalismo.faces.parameters.SphericalHarmonicsLight

case class SphericalHarmonicsLegacyFormat(coefficients: IndexedSeq[EuclideanVector[_3D]]) {
  def toSphericalHarmonics() = {
    SphericalHarmonicsLight(SphericalHarmonicsLegacyFormat.convertJzToSHL(coefficients))
  }
}

object SphericalHarmonicsLegacyFormat {
  def apply(sphericalHarmonicsLight: SphericalHarmonicsLight) = new SphericalHarmonicsLegacyFormat(
    convertSHLToJz(sphericalHarmonicsLight.coefficients)
  )

  def convertSHLToJz(shl: IndexedSeq[EuclideanVector[_3D]]): IndexedSeq[EuclideanVector[_3D]] = {
    if (shl.isEmpty)
      shl
    else if (shl.size > 9)
      throw new IllegalArgumentException(
        "SHL json writer (V1): cannot write more than 9 coefficients in jz format (use modern version)"
      )
    else {
      // 1 - 9 coefficients
      val permutation = IndexedSeq(0, 3, 1, 2, 6, 5, 7, 4, 8).take(shl.size)
      permutation map (i => shl(i))
    }
  }

  def convertJzToSHL(shl: IndexedSeq[EuclideanVector[_3D]]): IndexedSeq[EuclideanVector[_3D]] = {
    if (shl.isEmpty)
      shl
    else if (shl.size > 9)
      throw IllegalArgumentException(
        "SHL json reader (V1): cannot read more than 2 bands (9 coeffs), there is no conversion from jz to our format"
      )
    else {
      // 1 - 0 coeffs
      val permutation = IndexedSeq(0, 2, 3, 1, 7, 5, 4, 6, 8).take(shl.size)
      permutation map (i => shl(i))
    }
  }

  implicit val sphericalHarmonicsLegacyMapper: upickle.default.ReadWriter[SphericalHarmonicsLegacyFormat] =
    upickle.default
      .readwriter[List[List[Double]]]
      .bimap[SphericalHarmonicsLegacyFormat](
        leg => leg.coefficients.map(_.toArray.toList).toList,
        ll => SphericalHarmonicsLegacyFormat(ll.map(l => EuclideanVector[_3D](l.toArray)).toIndexedSeq)
      )
}
