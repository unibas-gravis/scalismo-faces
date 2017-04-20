package scalismo.faces

/**
  * Created by eggebe01 on 4/20/17.
  */


import breeze.linalg.DenseVector
import scalismo.common.{DiscreteDomain, DiscreteField, UnstructuredPointsDomain, Vectorizer}
import scalismo.faces.image.PixelImage
import scalismo.geometry._
import scalismo.statisticalmodel.{ModelHelpers, MultivariateNormalDistribution}



object BaselIlluminationPrior extends App {


  import java.io.File
  import scalismo.faces.io.msh.MSHMeshIO
  import scalismo.faces.io.RenderParameterIO
  import scalismo.utils.Random
  import scalismo.faces.color.RGBA
  import scalismo.faces.gui.GUIBlock._
  import scalismo.faces.gui.ImagePanel
  import scalismo.faces.parameters.{RenderParameter, SphericalHarmonicsLight, ParametricRenderer}

  scalismo.initialize()
  implicit val rnd = Random(1986)

  def fromBreezeVector(dv: DenseVector[Double]): SphericalHarmonicsLight = {
    require(dv.length % 3 == 0, "length of array to build SHLight must be a multiple of 3")
    val grouped = dv.toArray.grouped(3).map(g => Vector[_3D](g)).toIndexedSeq
    SphericalHarmonicsLight(grouped)
  }






  val sphere = MSHMeshIO.read(new File("/home/eggebe01/work/illuminationPrior/release//sphere.msh.gz")).get.colorNormalMesh.get
  val rpsAmbient = RenderParameter.defaultSquare.withEnvironmentMap(SphericalHarmonicsLight.ambientWhite)
  val imageAmbient = ParametricRenderer.renderParameterMesh(rpsAmbient, sphere)


  val targetPanel = ImagePanel(imageAmbient, RGBA.Black.toAWTColor)
  targetPanel.displayIn("Basel Illumination Prior 2017")
  //Thread.sleep(1000)

  val dir = "/home/eggebe01/work/illuminationPrior/release/toPublish/parameters/"
  val files = new File(dir).listFiles.filter(_.getName.endsWith(".rps")).toIndexedSeq

  val rndIllPath =  Random.randomGenerator.scalaRandom.shuffle(files.toList).head
  val rndIllRps = RenderParameterIO.read(rndIllPath).get
  val rndIllImg = ParametricRenderer.renderParameterMesh(rndIllRps, sphere)
  targetPanel.updateImage(rndIllImg)


  for( a <- 1 to 10){
    val rndIllPath =  Random.randomGenerator.scalaRandom.shuffle(files.toList).head
    val rndIllRps = RenderParameterIO.read(rndIllPath).get
    val rndIllImg = ParametricRenderer.renderParameterMesh(rndIllRps, sphere)
    targetPanel.updateImage(rndIllImg)
    //Thread.sleep(1000)
  }

  var allIll = files.map(f => {
    val rps = RenderParameterIO.read(f).get
    rps.environmentMap
  })

  var allIllData = allIll.map(i => i.toBreezeVector)

  val mnd = MultivariateNormalDistribution.estimateFromData(allIllData)




  val meanIll = fromBreezeVector(mnd.mean)
  val meanRps = RenderParameter.defaultSquare.withEnvironmentMap(meanIll)
  val meanIllImg = ParametricRenderer.renderParameterMesh(meanRps, sphere)
  targetPanel.updateImage(meanIllImg)

  def renderingFromIllumination(coeffs : DenseVector[Double]) : PixelImage[RGBA] = {
    val meanIll = fromBreezeVector(coeffs)
    val meanRps = RenderParameter.defaultSquare.withEnvironmentMap(meanIll)
    ParametricRenderer.renderParameterMesh(meanRps, sphere)
  }


  val randomIllImg = renderingFromIllumination(mnd.sample)
  targetPanel.updateImage(randomIllImg)


  val pcs: Seq[(DenseVector[Double], Double)] = mnd.principalComponents

  val firstPlus = mnd.mean + 3.0 * pcs(0)._1
  val firstPlusImg = renderingFromIllumination(firstPlus)
  val firstMinus = mnd.mean - 3.0 * pcs(0)._1
  val firstMinusImg = renderingFromIllumination(firstMinus)

  val firstMinusPanel = ImagePanel(firstMinusImg, RGBA.Black.toAWTColor)
  val meanPanel = ImagePanel(meanIllImg, RGBA.Black.toAWTColor)
  val firstPlusPanel = ImagePanel(firstPlusImg, RGBA.Black.toAWTColor)
  shelf(
    firstMinusPanel,
    meanPanel,
    firstPlusPanel
  ).displayIn("Basel Illumination Prior 2017, 1st Principal Component")


}
