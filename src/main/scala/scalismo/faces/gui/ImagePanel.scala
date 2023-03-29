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

package scalismo.faces.gui

import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import javax.swing.{JPanel, SwingUtilities}

import scalismo.faces.image.{BufferedImageConverter, PixelImage}

import scala.annotation.implicitNotFound

/** JPanel to display an image, the image is requested on each update */
class ImagePanel[A](var image: PixelImage[A], bgColor: Option[java.awt.Color] = None)(implicit
  conv: BufferedImageConverter[A]
) extends JPanel {
  setPreferredSize(new Dimension(image.width, image.height))

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val bufferedImage: BufferedImage = conv.toBufferedImage(image)
    val dx = (getWidth - bufferedImage.getWidth) / 2
    val dy = (getHeight - bufferedImage.getHeight) / 2
    bgColor match {
      case Some(color) => g.drawImage(bufferedImage, dx, dy, color, null)
      case None        => g.drawImage(bufferedImage, dx, dy, null)
    }
  }

  /** update the image (repaints this panel) */
  def updateImage(image: PixelImage[A]): Unit = {
    this.image = image
    repaint()
  }

  /** register an action to take on click */
  def onImageClick(actionLeftClick: (java.awt.Point) => Unit, actionRightClick: (Point) => Unit): Unit =
    addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        if (SwingUtilities.isLeftMouseButton(e)) actionLeftClick(e.getPoint)
        else if (SwingUtilities.isRightMouseButton(e)) actionRightClick(e.getPoint)
      }
    })
}

object ImagePanel {
  @implicitNotFound(
    "Can only display images with known conversion to BufferedImage, currently this is RGB, RGBA, Double, Float"
  )
  @deprecated(
    "width and height are ignored: use image of proper width and height, for a fixed size container use sizedContainer",
    "0.7"
  )
  def apply[A](width: Int, height: Int, image: PixelImage[A])(implicit conv: BufferedImageConverter[A]) =
    new ImagePanel(image)

  @implicitNotFound(
    "Can only display images with known conversion to BufferedImage, currently this is RGB, RGBA, Double, Float"
  )
  def apply[A](image: PixelImage[A])(implicit conv: BufferedImageConverter[A]) = new ImagePanel(image)

  /** creates a panel to display an image with a defined background color */
  @implicitNotFound(
    "Can only display images with known conversion to BufferedImage, currently this is RGB, RGBA, Double, Float"
  )
  def apply[A](image: PixelImage[A], bgColor: java.awt.Color)(implicit conv: BufferedImageConverter[A]) =
    new ImagePanel(image, Some(bgColor))
}
