/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.image

import scalismo.common.ComponentRepresentation
import scalismo.faces.image.filter.ImageFilter

import scala.reflect.ClassTag

object ChannelOperations {
  def fromMultiChannel[A: ClassTag](
    image: PixelImage[Array[Double]]
  )(implicit vec: ComponentRepresentation[A]): PixelImage[A] = {
    image.map(a => vec.fromArray(a))
  }

  def filterChannels[A: ClassTag](image: PixelImage[A], channelFilter: ImageFilter[Double, Double])(implicit
    vec: ComponentRepresentation[A]
  ): PixelImage[A] = {
    val channels = decomposeChannels(image)
    val filteredChannels = channels.map(c => channelFilter.filter(c))
    composeChannels(filteredChannels)
  }

  def decomposeChannels[A](
    image: PixelImage[A]
  )(implicit vec: ComponentRepresentation[A]): IndexedSeq[PixelImage[Double]] = {
    val mcImage = multiChannelImage(image)
    for (i <- 0 until vec.size) yield mcImage.map(a => a(i))
  }

  def multiChannelImage[A](
    image: PixelImage[A]
  )(implicit vec: ComponentRepresentation[A]): PixelImage[Array[Double]] = {
    image.map(p => vec.toArray(p))
  }

  def composeChannels[A: ClassTag](
    images: IndexedSeq[PixelImage[Double]]
  )(implicit vec: ComponentRepresentation[A]): PixelImage[A] = {
    require(images.nonEmpty, "no channels")
    val domain = images.head.domain
    require(images.forall(i => i.domain == domain), "domains are not equal")
    PixelImage(domain,
               (x, y) => {
                 vec.fromComponents(i => images(i)(x, y))
               }
    )
  }
}
