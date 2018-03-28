package scalismo.faces.momo

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{UnstructuredPointsDomain, Vectorizer}
import scalismo.faces.color.RGB
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.mesh.MeshCompactifier
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess

/** Mask a MoMo or DiscreteLowRankGaussianProcess according to supplied mask in the form of a MeshCompactifier.*/
case class MoMoMasker(op: MeshCompactifier) {
  val maskedMesh = op.transformedMesh
  val newIdx = maskedMesh.triangulation.pointIds

  /* Mask a model by removing entries from the given model mean and eigenvectors and creating a new model from them. */
  def maskDiscreteLowRankGaussianProcess[A](m: PancakeDLRGP[_3D, UnstructuredPointsDomain[_3D], A])(implicit vectorizer: Vectorizer[A]): DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], A] = {

    val domain = UnstructuredPointsDomain[_3D](maskedMesh.position.pointData)

    val gp = m.gpModel
    val meanNotMasked = gp.meanVector.toArray.grouped(vectorizer.dim).toIndexedSeq

    val meanMasked = DenseVector(
      newIdx.flatMap { id => meanNotMasked(op.pointBackMap(id).id) }.toArray
    )

    val maskedBasisMatrix = DenseMatrix.zeros[Double](newIdx.length * vectorizer.dim, m.gpModel.basisMatrix.cols)
    newIdx.foreach { id =>
      for (d <- 0 until vectorizer.dim) {
        maskedBasisMatrix(id.id * vectorizer.dim + d, ::) := gp.basisMatrix(op.pointBackMap(id).id * vectorizer.dim + d, ::)
      }
    }
    DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], A](domain, meanMasked, gp.variance, maskedBasisMatrix)
  }

  def maskMoMo(model: MoMo): MoMo = {
    model match {
      case model: MoMoBasic => maskMoMoBasic(model)
      case model: MoMoExpress => maskMoMoExpress(model)
    }
  }

  def maskMoMoBasic(model: MoMoBasic): MoMoBasic = {
    val maskedModelShape: PancakeDLRGP[_3D, UnstructuredPointsDomain[_3D], Point[_3D]] = PancakeDLRGP(maskDiscreteLowRankGaussianProcess(model.shape))
    val maskedModelColor: PancakeDLRGP[_3D, UnstructuredPointsDomain[_3D], RGB] = PancakeDLRGP(maskDiscreteLowRankGaussianProcess(model.color))
    MoMo(maskedMesh, maskedModelShape, maskedModelColor)
  }

  def maskMoMoExpress(model: MoMoExpress): MoMoExpress = {
    val maskedModelShape: PancakeDLRGP[_3D, UnstructuredPointsDomain[_3D], Point[_3D]] = PancakeDLRGP(maskDiscreteLowRankGaussianProcess(model.shape))
    val maskedModelColor: PancakeDLRGP[_3D, UnstructuredPointsDomain[_3D], RGB] = PancakeDLRGP(maskDiscreteLowRankGaussianProcess(model.color))
    val maskedModelExpressions: PancakeDLRGP[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]] = PancakeDLRGP(maskDiscreteLowRankGaussianProcess(model.expression))
    MoMo(maskedMesh, maskedModelShape, maskedModelColor, maskedModelExpressions, model.landmarks)
  }
}