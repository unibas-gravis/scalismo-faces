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
package scalismo.faces.mesh

import java.io.File

import scalismo.common.UnstructuredPointsDomain.Create
import scalismo.common.{DiscreteDomain, DiscreteField, PointId, UnstructuredPoints, UnstructuredPointsDomain}
import scalismo.faces.io.GravisArrayIO
import scalismo.geometry.{Dim, NDSpace, _3D}
import scalismo.mesh.TriangleMesh

import scala.language.higherKinds
import scala.io.Source
import scala.util.{Failure, Success, Try}


/**
  * Mask is a function from a position in a sequence to a boolean. A mask can be used to remove the same position
  * in many sequences of values with the same length. This is usefull if the ordering has a semantical meaning, as for
  * example in the case of the point or triangle lists in a TriangleMesh3D.
  *
  * @param entries Boolean values if the given position is active or not.
  */
case class BinaryMask(entries: IndexedSeq[Boolean]) extends (PointId => Boolean) {

  /**
    * Mapping assuming that the PointId values point directly to IndexedSet of the entries.
    */
  def apply(pid: PointId): Boolean = {
    entries(pid.id)
  }

  /**
    * Masks a DiscreteField according to the mask.
    */
  def cut[D <: Dim : NDSpace: Create, DDomain[A] <: DiscreteDomain[A], T](field: DiscreteField[D, DDomain, T])(implicit creator: UnstructuredPoints.Create[D]): DiscreteField[D, UnstructuredPointsDomain, T] = {
    require(field.domain.pointSet.numberOfPoints == entries.size,
      "The domain of the field and the mask need to have the same length!!!")
    val maskedPoints = cut(field.domain.pointSet.points.toIndexedSeq)
    val maskedData = cut(field.data)
    DiscreteField(UnstructuredPointsDomain(maskedPoints), maskedData)
  }

  /**
    * Selects entries of an IndexedSeq according to the mask.
    */
  def cut[T](iseq: IndexedSeq[T]): IndexedSeq[T] = {
    require(iseq.size == entries.size,
      "The IndexedSeq and the mask need to have the same length!!!")
    iseq.zip(entries).filter(e => e._2).map(e => e._1)
  }


  /**
    * Selects entries of a provided mask according to the mask.
    */
  def cut(mask: BinaryMask): BinaryMask = {
    require( entries.size == mask.entries.size,
      "When masking a mask, the two masks need to have the same size!!!")
    BinaryMask(cut(mask.entries))
  }

  /**
    * Emplaces the values of this mask on to the locations that are true on the provided mask.
    */
  def emplaceOnTruesOf(mask: BinaryMask): BinaryMask = {
    require( mask.entries.count(b => b) == entries.size,
      "The size of mask need to be equal to the entries in the original mask with value true!!!")

    val maskWithIndex = mask.entries.zipWithIndex
    val maskTrues = maskWithIndex.filter(e => e._1)
    val lookup = maskTrues.zip(entries).map{ case (trues,bool) => (trues._2,bool)}.toMap
    val newEntries = maskWithIndex.map{ e =>
      val pid = e._2
      lookup.getOrElse(pid,false)
    }
    BinaryMask(newEntries)
  }

  /**
    * Combines two mask by the logical AND between all corresponding entries.
    */
  def and(mask: BinaryMask): BinaryMask = {
    require(entries.size == mask.entries.size,
      "To calculate the logical AND between two masks they need to have the same length!!!")
    val newEntries = entries.zip(mask.entries).map(p => p._1 && p._2)
    BinaryMask(newEntries)
  }

  /**
    * Combines two mask by the logical OR between all corresponding entries.
    */
  def or(mask: BinaryMask): BinaryMask = {
    require(entries.size == mask.entries.size,
      "To calculate the logical OR between two masks they need to have the same length!!!")

    val newEntries = entries.zip(mask.entries).map(p => p._1 || p._2)
    BinaryMask(newEntries)
  }

  /**
    * Inverts a mask.
    */
  def inv(): BinaryMask = {
    BinaryMask(entries.map(!_))
  }

  /**
    * Write mask to file. The file is human readable.
    */
  def write(file: File): Try[Unit] = {
    val values = entries.map(if(_) 1 else 0)
    GravisArrayIO.write(values,file)
  }

}

/**
  * Companion object to construct masks from a pair of TriangleMesh[_3D] or UnstructuredPointsDomain.
  */
object BinaryMask {

  /**
    * Creates a mask from an original mesh and a masked mesh. The points of the masked subset need to be contained
    * exactly in the original. Neither the original nor the masked mesh can contain two points with exactly the same
    * location.
    *
    * @param original Larger mesh of the two.
    * @param masked Mesh that can be made from the original when masking it.
    */
  def createFromMeshes(original: TriangleMesh[_3D], masked: TriangleMesh[_3D]): BinaryMask = {
    createFromUnstructuredPoints(original.pointSet,masked.pointSet)
  }

  /**
    * Creates a mask from an original and a masked UnstructuredPointsDomain. See [[apply(TriangleMesh[_3D], TriangleMesh[_3D])]] for more
    * information.
    *
    * @param original Larger UnstructuredPointsDomain.
    * @param masked Masked version of the larger UnstructuredPointsDomain.
    */
  def createFromUnstructuredPoints(original: UnstructuredPoints[_3D], masked: UnstructuredPoints[_3D]): BinaryMask = {
    val originalPoints = original.points.toIndexedSeq
    val maskedPoints = masked.points.toIndexedSeq
    require(maskedPoints.forall( p => maskedPoints.count(q => q==p)==1 && originalPoints.count(q => q==p)==1),
      "Can not create a mask from meshes where multiple points are identical to the full accuracy of the used numerical type.!!!"
    )

    val flags = original.points.toIndexedSeq.map{ p =>
      if ( p == masked.findClosestPoint(p).point ) true
      else false
    }

    BinaryMask(flags)
  }

  /**
    * Load binary mask from a source.
    */
  def load(source: Source): Try[BinaryMask] = {
    val loaded = GravisArrayIO.readFromSource[Int](source)
    loaded match {
      case Success(mask) =>
        require (mask.forall (d => d == 0 || d == 1),
          "The function load supports only true binary masks with all entries either equal to 0 or 1!!!")
        Success(BinaryMask (mask.map (d => d == 1) ))
      case Failure(error) => Failure(error)
    }
  }

}
