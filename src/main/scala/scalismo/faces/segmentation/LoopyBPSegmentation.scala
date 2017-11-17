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


package scalismo.faces.segmentation

import java.io.File

import scalismo.faces.color.{ColorDistribution, GaussDist, RGB, UniformDist}
import scalismo.faces.image.{ImageBuffer, PixelImage}
import scalismo.faces.gui.GUIBlock._
import scalismo.faces.gui.{GUIFrame, ImagePanel}
import scalismo.faces.io.PixelImageIO
import scalismo.utils.Random

import scala.collection.immutable.IndexedSeq


// an implementation of loopy belief propagation for segmentation
object LoopyBPSegmentation {


  // the directions for message passing
  private sealed trait Direction {
    def toInt: Int
  }

  private object Direction {
    val allDirections = IndexedSeq(Right, Left, Down, Up)

    def fromIndex(index: Int): Direction = index match {
      case 0 => Right
      case 1 => Left
      case 2 => Down
      case 3 => Up
      case _ => throw new RuntimeException("unknown direction index")
    }
  }

  private case object Right extends Direction {
    override def toInt: Int = 0
  }

  private case object Left extends Direction {
    override def toInt: Int = 1
  }

  private case object Down extends Direction {
    override def toInt: Int = 2
  }

  private case object Up extends Direction {
    override def toInt: Int = 3
  }

  /** the segmentation label */
  case class Label(value: Int) extends AnyVal

  /** binary distribution, local information */
  type BinaryLabelDistribution = PixelImage[(Label, Label) => Double]

  private type MessageFieldBuf = ImageBuffer[Array[LabelDist]]

  /** MRF segmentation with Loopy Belief Propagation
    * Builds simple Gaussian color models for each label iteratively
    *
    * @param image image to segment
    * @param labelInit initial label where known
    * @param numLabels number of labels in use */

  def segmentImage(image: PixelImage[RGB],
                   labelInit: PixelImage[Option[Label]],
                   binaryDistribution: PixelImage[(Label, Label) => Double],
                   numLabels: Int, numIterations: Int,
                   gui: Boolean = false): PixelImage[LabelDist] = {
    require(labelInit.values.forall(i => i.forall(_.value < numLabels)))

    // init: color models and start labels
    val initialSegmentation = labelInit.map{init => LabelDist.fromSingleLabel(init.getOrElse(Label(numLabels-1)), numLabels, 0.01)}
    var colorDists = estimateColorDists(image, initialSegmentation)

    // initialize message field: constant messages, unknown distributions yet
    val messageField = ImageBuffer.makeInitializedBuffer(image.width, image.height)(Array.fill(Direction.allDirections.length)(LabelDist.constant(numLabels)))

    // calculate local messages: according to color distributions
    var localMessages = image.map{c => LabelDist(colorDists.map{dist => dist.evaluate(c)})}.map{_.normalized}
    val counterLabel = label("init")
    val guiFrame = if (gui) {
      // init GUI

      stack(
        counterLabel,
        shelf(
          ImagePanel(visSampleImage(localMessages, colorDists)),
          ImagePanel(visRGBSegImage(localMessages)),
          ImagePanel(visRGBSegImage(localMessages))
        )).displayIn("segmentation")
    }
    else
      GUIFrame("dummy")

    // unsafe view on result
    val unsafeMsgFieldView = messageField.toUnsafeImage

    // do message passing
    for(i <- 0 until numIterations) {
      localMessages = image.map{c => LabelDist(colorDists.map{dist => dist.evaluate(c)})}.map{_.normalized}
      loopyBeliefPropagationPass(messageField, localMessages, binaryDistribution, numLabels)
      val belief = calculateBelief(unsafeMsgFieldView, localMessages)

      if(gui) {
        // gui update
        if (i%1 == 0) {
          colorDists = estimateColorDists(image, belief)
          println(colorDists)
        }

        counterLabel.setText("iteration: " + i)
        stack(
          counterLabel,
          shelf(
            ImagePanel(visSampleImage(belief, colorDists)),
            ImagePanel(visRGBSegImage(belief)),
            ImagePanel(visRGBSegImage(localMessages)))).displayIn(guiFrame)
      }
    }

    // calculate final belief at each node / pixel
    calculateBelief(messageField.toImage, localMessages)
  }


  def segmentImageFromProb(image: PixelImage[RGB],
                           labelInit: PixelImage[Label],
                           bgProb: PixelImage[Double],
                           fgProb: PixelImage[Double],
                           binaryDistribution: PixelImage[(Label, Label) => Double],
                           numLabels: Int, numIterations:
                           Int, gui: Boolean = false): PixelImage[LabelDist] = {
    // init: color models and start labels
    val initialSegmentation = labelInit.map{init => LabelDist.fromSingleLabel(init, numLabels, 0.01)}
    var colorDists = estimateColorDists(image, initialSegmentation)

    // initialize message field: constant messages, unknown distributions yet
    val messageField = ImageBuffer.makeInitializedBuffer(image.width, image.height)(Array.fill(Direction.allDirections.length)(LabelDist.constant(numLabels)))

    // calculate local messages: according to color distributions
    val localMessages: PixelImage[LabelDist] = bgProb.zip(fgProb).map(a => LabelDist(IndexedSeq(a._1, a._2))).map{_.normalized}

    // init GUI
    val counterLabel = label("init")
    val guiFrame = if(gui) {
      stack(
        counterLabel,
        shelf(
          ImagePanel(visSampleImage(localMessages, colorDists)),
          ImagePanel(visRGBSegImage(localMessages)),
          ImagePanel(visRGBSegImage(localMessages))
        )).displayIn("segmentation")
    }
    else
      GUIFrame("dummy")

    // unsafe view on result
    val unsafeMsgFieldView = messageField.toUnsafeImage

    // do message passing
    for(i <- 0 until numIterations) {
      //localMessages = image.map{c => LabelDist(colorDists.map{dist => dist.evaluate(c)})}.map{_.normalized}
      loopyBeliefPropagationPass(messageField, localMessages, binaryDistribution, numLabels)
      val belief = calculateBelief(unsafeMsgFieldView, localMessages)
      if (gui){
        // gui update
        if (i%1 == 0) {
          colorDists = estimateColorDists(image, belief)
          println(colorDists)
        }

        counterLabel.setText("iteration: " + i)
        stack(
          counterLabel,
          shelf(
            ImagePanel(visSampleImage(belief, colorDists)),
            ImagePanel(visRGBSegImage(belief)),
            ImagePanel(visRGBSegImage(localMessages)))).displayIn(guiFrame)
      }
    }

    // calculate final belief at each node / pixel
    calculateBelief(messageField.toImage, localMessages)
  }

  def visSampleImage(labelImage: PixelImage[LabelDist], colorDists: IndexedSeq[ColorDistribution]): PixelImage[RGB] = {
    labelImage.map { labels => colorDists(labels.maxLabel).sample }
  }

  def colorMapImage(labelImage: PixelImage[LabelDist]): PixelImage[RGB] = {
    labelImage.map {
      _.maxLabel
    }.map {
      case 0 => RGB.White
      case 1 => RGB.Black
      case 2 => RGB(1, 0, 0)
      case 3 => RGB(0, 1, 0)
      case _ => RGB(0, 0, 1)
    }
  }

  def visRGBSegImage(labelImage: PixelImage[LabelDist]): PixelImage[RGB] = {
    labelImage.map { l => l.length match {
      case 2 => RGB(l(Label(0)), l(Label(1)), 0)
      case _ => RGB(l(Label(0)), l(Label(1)), l(Label(2)))
    }
    }
  }

  // helper class to handle the distribution of the label per pixel
  case class LabelDist(private val values: IndexedSeq[Double]) {
    def toLogLabelDist: LogLabelDist = LogLabelDist(values.map(math.log))

    def length = values.length

    def apply(label: Label) = values(label.value)

    def normalized = {
      val s = values.sum
      LabelDist(values.map { v => v / s })
    }

    def maxLabel = values.zipWithIndex.maxBy {
      _._1
    }._2

    def marginalize(p: Label => Double): Double = {
      normalized.values.zipWithIndex.map { case (v, i) => v * p(Label(i)) }.sum
    }

    def *(other: LabelDist): LabelDist = {
      require(other.length == length, "labeldist *")
      LabelDist(values.zip(other.values).map { case (v, w) => v * w })
    }
  }

  object LabelDist {
    def constant(numLabels: Int): LabelDist = LabelDist(IndexedSeq.fill(numLabels)(1.0 / numLabels))

    def fromSingleLabel(l: Label, numLabels: Int, eps: Double = 1e-3): LabelDist = LabelDist(IndexedSeq.tabulate(numLabels) { j => if (j == l.value) 1.0 - numLabels * eps else eps })

    def tabulate(numLabels: Int)(f: Label => Double): LabelDist = LabelDist(IndexedSeq.tabulate(numLabels) { l => f(Label(l)) })
  }

  case class LogLabelDist(private val values: IndexedSeq[Double]) {

    def length = values.length

    def apply(label: Label) = values(label.value)

    def normalized: LogLabelDist = {
      val maxlog = values.max
      val logPTot = math.log(values.map(x => math.exp(x - maxlog)).sum) + maxlog
      LogLabelDist(values.map(_ - logPTot))
    }

    def maxLabel = values.zipWithIndex.maxBy {
      _._1
    }._2

    def marginalizeLog(logP: Label => Double): Double = {
      val maxlog = values.max
      math.log(normalized.values.zipWithIndex.map { case (v, i) => math.exp(v - maxlog + logP(Label(i))) }.sum) + maxlog
    }

    def marginalize(p: Label => Double): Double = {
      val maxlog = values.max
      math.exp(math.log(normalized.values.zipWithIndex.map { case (v, i) => math.exp(v - maxlog) * p(Label(i)) }.sum) + maxlog)
    }

    def toLabelDist: LabelDist = {
      LabelDist(values.map(math.exp))
    }

    def *(other: LogLabelDist): LogLabelDist = {
      require(other.length == length, "length mult")
      LogLabelDist(values.zip(other.values).map { case (v, w) => v + w })
    }

    def /(other: LogLabelDist): LogLabelDist = {
      require(other.length == length, "length divide")
      LogLabelDist(values.zip(other.values).map { case (v, w) => v - w })
    }
  }

  object LogLabelDist {
    def apply(labelDist: LabelDist): LogLabelDist = labelDist.toLogLabelDist

    def constant(numLabels: Int): LogLabelDist = LogLabelDist(IndexedSeq.fill(numLabels)(1.0 / numLabels))

    def fromSingleLabel(l: Label, numLabels: Int, eps: Double = 1e-3): LogLabelDist = LabelDist.fromSingleLabel(l, numLabels, eps).toLogLabelDist

    def tabulate(numLabels: Int)(f: Label => Double): LogLabelDist = LogLabelDist(IndexedSeq.tabulate(numLabels) { l => f(Label(l)) })

  }

  def binDist(pEqual: Double, numLabels: Int, width: Int, height: Int): BinaryLabelDistribution = {
    val pElse = (1 - pEqual) / (numLabels - 1)
    def binDist(k: Label, l: Label) = if (k == l) pEqual else pElse
    PixelImage.view(width, height, (x, y) => binDist)
  }


  type RegionTypeDist = LabelDist
  type LogRegionTypeDist = LogLabelDist


  /** message field update, single iteration, includes all directions */
  private def loopyBeliefPropagationPass(messageField: MessageFieldBuf,
                                         localMessages: PixelImage[LabelDist],
                                         binaryDist: BinaryLabelDistribution,
                                         numLabels: Int): MessageFieldBuf = {

    // 4 passes: right, left, down, up
    messagePass(messageField, localMessages, binaryDist, numLabels, Right)
    messagePass(messageField, localMessages, binaryDist, numLabels, Left)
    messagePass(messageField, localMessages, binaryDist, numLabels, Down)
    messagePass(messageField, localMessages, binaryDist, numLabels, Up)
    messageField
  }


  /** message field update with prior, single iteration, includes all directions and prior */
  private def loopyBeliefPropagationPassWithPrior(messageField: MessageFieldBuf,
                                                  localMessages: PixelImage[LabelDist],
                                                  incomingRegionPriorMessages: ImageBuffer[RegionTypeDist],
                                                  outgoingRegionPriorMessages: ImageBuffer[LabelDist],
                                                  binaryDist: BinaryLabelDistribution,
                                                  numLabels: Int,
                                                  regionPrior: IndexedSeq[PixelImage[LabelDist]]): Unit = {
    loopyBeliefPropagationPass(messageField, localMessages, binaryDist, numLabels)
    priorMessagePass(incomingRegionPriorMessages, outgoingRegionPriorMessages,  messageField, localMessages, regionPrior, numLabels)
  }

  /** calculate a belief update message in given direction */
  private def calculateMessage(messageField: MessageFieldBuf,
                               localMessages: PixelImage[LabelDist],
                               binaryDist: BinaryLabelDistribution,
                               numLabels: Int,
                               x: Int,
                               y: Int,
                               direction: Direction): LabelDist = {
    // gather all incoming messages: ignore the one from the current direction
    val incomingDirections = Direction.allDirections.filter {
      _ != direction
    }
    val msgs = incomingDirections.map { direction => messageField(x, y)(direction.toInt) }
    // product of all incoming messages
    val prodMsg = msgs.foldLeft(localMessages(x, y)) { (prod, msg) => prod * msg }
    // marginalize with binary distribution here, for each label -> outgoing message
    val outMessage = LabelDist.tabulate(numLabels) { label => prodMsg.marginalize(binaryDist(x, y)(label, _)) }
    outMessage
  }

  /** calculate the belief from a message field */
  private def calculateBelief(messageField: PixelImage[Array[LabelDist]], localMessages: PixelImage[LabelDist]): PixelImage[LabelDist] = {
    messageField.zip(localMessages).map{case(msgs, local) =>
      msgs.foldLeft(local){(prod, msg) => prod * msg}.normalized
    }
  }

  /** calculate the belief from a message field including prior messages*/
  private def calculateBeliefWithPrior(messageField: PixelImage[Array[LabelDist]],
                                       incomingRegionPriorMessages: PixelImage[RegionTypeDist],
                                       outgoingRegionPriorMessages: PixelImage[LabelDist],
                                       localMessages: PixelImage[LabelDist]): (PixelImage[LabelDist], RegionTypeDist) = {
    val labelDist = messageField.zip(localMessages).zip(outgoingRegionPriorMessages)map { case ((msgs, local),regionPrior) =>
      msgs.foldLeft(local) { (prod, msg) => prod * msg * regionPrior }.normalized
    }
    val priorIncomingProduct: RegionTypeDist = incomingRegionPriorMessages.values.map(_.toLogLabelDist).reduce(_ * _).normalized.toLabelDist


    (labelDist, priorIncomingProduct)
  }

  /** passes the message of the prior per label, the prior acts as regionprior*/
  private def priorMessagePass(incomingRegionPriorMessages: ImageBuffer[RegionTypeDist],
                               outgoingRegionPriorMessages: ImageBuffer[LabelDist],
                               messageField: MessageFieldBuf,
                               localMessages: PixelImage[LabelDist],
                               regionPrior: IndexedSeq[PixelImage[LabelDist]],
                               numLabels: Int): Unit = {


    val numBeards = regionPrior.length

    // calculcate messages sent to prior
    // collect all incoming messages from neighbours
    val lateralMessages = messageField.toUnsafeImage.map(allMessages => allMessages.reduce(_ * _))
    // multiply with local messages
    val allMessages = lateralMessages.zip(localMessages).map { case (lat, loc) => (lat * loc).normalized }

    //  ImagePanel(visRGBSegImage(allMessages)).displayInNewFrame("allmessages")

    // marginalize over all possible segmentation labels and normalize
    incomingRegionPriorMessages.transformWithIndexParallel { (x, y, _) =>
      LabelDist.tabulate(numBeards) { (c: Label) =>
        allMessages(x, y).marginalize((z: Label) => regionPrior(c.value)(x, y)(z))
      }.normalized
    }

    // multiply all messages (leaf one out is done by dividing later)
    val allRegionProduct: LogRegionTypeDist = incomingRegionPriorMessages.toUnsafeImage.values.map(_.toLogLabelDist).reduce(_ * _).normalized


    outgoingRegionPriorMessages.transformWithIndexParallel { (x, y, _) =>
      val incomingMessages: LogRegionTypeDist = allRegionProduct / incomingRegionPriorMessages(x, y).toLogLabelDist
      LabelDist.tabulate(numLabels) { (z: Label) =>
        incomingMessages.marginalize((c: Label) => regionPrior(c.value)(x, y)(z))
      }.normalized
    }
  }

  /** a single message pass in given direction */
  private def messagePass(messageField: MessageFieldBuf, localMessages: PixelImage[LabelDist], binaryDist: BinaryLabelDistribution, numLabels: Int, direction: Direction): Unit = {
    direction match {
      case Right => // do rows in parallel, sequential along row
        for {row <- (0 until messageField.height).par
             x <- 0 until messageField.width - 1} {
          // store message: is incoming from the left in next node
          messageField(x + 1, row)(Left.toInt) = calculateMessage(messageField, localMessages, binaryDist, numLabels, x, row, Right)
        }
      case Left => // do rows in parallel, sequential along row
        for {row <- (0 until messageField.height).par
             x <- messageField.width - 1 to 1 by -1} {
          messageField(x - 1, row)(Right.toInt) = calculateMessage(messageField, localMessages, binaryDist, numLabels, x, row, Left)
        }
      case Down => // do columns in parallel, sequential along column
        for {col <- (0 until messageField.width).par
             y <- 0 until messageField.height - 1} {
          messageField(col, y + 1)(Up.toInt) = calculateMessage(messageField, localMessages, binaryDist, numLabels, col, y, Down)
        }
      case Up => // do columns in parallel, sequential along column
        for {col <- (0 until messageField.width).par
             y <- messageField.height - 1 to 1 by -1} {
          messageField(col, y - 1)(Down.toInt) = calculateMessage(messageField, localMessages, binaryDist, numLabels, col, y, Up)
        }
    }
  }

  def estimateColorDists(image: PixelImage[RGB], labelDist: PixelImage[LabelDist]): IndexedSeq[ColorDistribution] = {
    // max only estimation currently
    val maxLabels = labelDist.map {
      _.maxLabel
    }
    val maxLabel: Int = labelDist.map {
      _.length
    }.values.max
    // estimate for each label
    IndexedSeq.tabulate(maxLabel) { n =>
      val colors = image.zip(maxLabels).values.collect { case (color, label) if label == n => color }.toIndexedSeq
      if (colors.nonEmpty)
        GaussDist(colors)
      else
        UniformDist
    }
  }
}