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

package scalismo.faces.io

import java.io._
import java.net.URI

import scalismo.faces.FacesTestSuite
import scalismo.color.{RGB, RGBA}
import scalismo.faces.image.PixelImageDomain
import scalismo.faces.io.renderparameters.{RenderParameterJSONFormat, RenderParameterJSONFormatV4}
import scalismo.faces.mesh._
import scalismo.faces.parameters._
import scalismo.faces.render._
import scalismo.faces.utils.ResourceManagement
import scalismo.geometry._
import scalismo.mesh.{SurfacePointProperty, VertexColorMesh3D}
import spray.json._

import scala.io.Source
import scala.reflect.ClassTag

class ParametersIOTests extends FacesTestSuite {

  private def d = rnd.scalaRandom.nextDouble()
  private def randomAngle(range: Double = 2 * math.Pi) = d * range
  private def randomList(n: Int) = IndexedSeq.fill(n)(rnd.scalaRandom.nextGaussian())

  def randomParameter(): RenderParameter = {
    val w = rnd.scalaRandom.nextInt(2048)
    val h = rnd.scalaRandom.nextInt(2048)
    val n = rnd.scalaRandom.nextInt(400)

    RenderParameter(
      pose = Pose(
        scaling = d * 2 + 1f,
        translation = Vector(d * 100, d * 100, -10000 * d),
        pitch = randomAngle(),
        yaw = randomAngle(),
        roll = randomAngle()),
      view = ViewParameter(
        translation = Vector(d * 100, d * 100, d * 1000),
        pitch = randomAngle(),
        yaw = randomAngle(),
        roll = randomAngle()),
      camera = Camera(
        focalLength = d * 40,
        principalPoint = Point(d * 2 - 1, d * 2 - 1),
        sensorSize = Vector(d * 100, d * 100),
        near = d * 0.1,
        far = 1e12,
        orthographic = rnd.scalaRandom.nextBoolean()),
      environmentMap = SphericalHarmonicsLight(IndexedSeq.fill(9)(Vector(d, d, d))),
      directionalLight = DirectionalLight.off,
      momo = MoMoInstance(
        shape = randomList(n),
        color = randomList(n),
        expression = randomList(n),
        randomURI(20)),
      imageSize = ImageSize(
        width = w,
        height = h),
      colorTransform = ColorTransform(
        gain = randomRGB,
        colorContrast = d,
        offset = randomRGB))
  }

  def vec2Close(v1: Vector[_2D], v2: Vector[_2D]): Unit = {
    math.abs(v1.x - v2.x) should be < 1e-5
    math.abs(v1.y - v2.y) should be < 1e-5
  }

  def cameraEqual(c1: Camera, c2: Camera): Unit = {
    c1.far shouldBe c2.far
    c1.focalLength shouldBe c2.focalLength
    c1.near shouldBe c2.near
    c1.orthographic shouldBe c2.orthographic
    c1.principalPoint shouldBe c2.principalPoint
    vec2Close(c1.sensorSize, c2.sensorSize)
  }

  def viewEqual(v1: ViewParameter, v2: ViewParameter): Unit = {
    v1.pitch shouldBe v2.pitch
    v1.roll shouldBe v2.roll
    v1.yaw shouldBe v2.yaw
    v1.translation shouldBe v2.translation
  }

  def identical(p1: RenderParameter, p2: RenderParameter): Unit = {
    cameraEqual(p1.camera, p2.camera)
    p1.colorTransform shouldBe p2.colorTransform
    p1.momo shouldBe p2.momo
    p1.imageSize shouldBe p2.imageSize
    p1.environmentMap shouldBe p2.environmentMap
    p1.directionalLight shouldBe p2.directionalLight
    p1.pose shouldBe p2.pose
    p1.view shouldBe p2.view
  }

  def writeReadTest[A: ClassTag](a: A)(implicit format: JsonFormat[A]): Unit = {
    val tag = implicitly[ClassTag[A]]
    it(s"consistently writes/reads ${tag.runtimeClass.getName}"){
      a.toJson.convertTo[A] shouldBe a
    }
  }

  // choose a random parameter for this test: "ensures" different values for each field
  val randomParam: RenderParameter = randomParameter()

  describe("RenderParameters JSON Formats V3") {

    import scalismo.faces.io.renderparameters.RenderParameterJSONFormatV3._

    describe("View parameterization") {
      writeReadTest(ViewParameter(randomVector3D, randomDouble, randomDouble, randomDouble))
    }

    describe("RenderObject JSON format") {
      val mesh: VertexColorMesh3D = randomGridMesh()

      writeReadTest(MeshVertexColor(mesh))

      writeReadTest(MeshColorNormals(ColorNormalMesh3D(mesh)))

      writeReadTest(MeshFile(randomURI(10)))

      writeReadTest(MoMoInstance(
        shape = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
        color = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
        expression = IndexedSeq.empty,
        modelURI = randomURI(10)))
    }

    describe("JSON format for MeshSurfaceProperties") {
      val mesh: VertexColorMesh3D = randomGridMesh()
      writeReadTest(mesh.color)

      val textureMapping: SurfacePointProperty[Point[_2D]] = {
        val boundingBox = mesh.shape.boundingBox
        val ll = boundingBox.origin
        val ex = boundingBox.extent

        def pnorm(pt: Point[_3D]): Point[_2D] = {
          val cent = pt - ll
          Point(cent.x / ex.x, cent.y / ex.y)
        }

        val uvPoints = mesh.shape.pointSet.points.map{pnorm}.toIndexedSeq
        SurfacePointProperty(mesh.shape.triangulation, uvPoints)
      }

      val colTex = TextureMappedProperty.fromSurfaceProperty(mesh.color, textureMapping, PixelImageDomain(10, 10), RGBA(1, 1, 0, 1))
      writeReadTest(colTex)

      val vcPerTriangle = VertexPropertyPerTriangle.fromPointProperty(mesh.shape.vertexNormals)
      writeReadTest(vcPerTriangle)

      val indirect = IndirectProperty(mesh.shape.triangulation, IndexedSeq.fill(mesh.shape.triangulation.triangles.length)(0), IndexedSeq(mesh.color))
      writeReadTest(indirect)
    }

    describe("Illumination JSON format") {
      writeReadTest(DirectionalLight(randomRGB, randomRGB, randomVector3D.normalize, randomRGB, rnd.scalaRandom.nextDouble()))
      writeReadTest(randomParam.environmentMap)
    }

    describe("SceneParameter") {
      it("can convert between RenderParameter and SceneParameter") {
        val sp = SceneParameter(randomParam)
        val rp = sp.toRenderParameter.get
        rp shouldBe randomParam
      }

      it("can write/read a SceneParameter") {
        val sp = SceneParameter(randomParam)

        val f = File.createTempFile("ParametersIOTest", ".rps")
        f.deleteOnExit()
        ResourceManagement.using(new PrintWriter(new FileOutputStream(f))) { wr =>
          wr.print(sp.toJson.prettyPrint)
        }

        val rsp = Source.fromFile(f).mkString.parseJson.convertTo[SceneParameter]
        rsp shouldBe sp
      }
    }

    describe("SceneTree") {
      def randomPose() = Pose(randomDouble, randomVector3D, randomAngle() - math.Pi, randomAngle() - math.Pi, randomAngle() - math.Pi)

      def checkTrafoEqual(t1: Transform3D, t2: Transform3D): Unit = {
        def d() = 2 * rnd.scalaRandom.nextDouble() - 1
        def randomPoint() = Point3D(d(), d(), d())
        for(i <- 0 until 20) {
          val p = randomPoint()
          val v = p.toVector
          t1(v) shouldBe t2(v)
          t1(p) shouldBe t2(p)
        }
      }

      def quaternionEqual(q1: Quaternion, q2: Quaternion): Unit = {
        if (q1.r * q2.r >= 0) {
          q1.r shouldBe q2.r +- 1e-14
          (q1.v - q2.v).norm should be < 1e-14
        } else
          quaternionEqual(q1, -q2)
      }

      def rotationEqual(r1: Rotation3D, r2: Rotation3D): Unit = {
        val q1 = r1.quaternion
        val q2 = r2.quaternion
        quaternionEqual(q1, q2)
      }

      def poseEqual(p1: Pose, p2: Pose): Unit = {
        p1.translation shouldBe p2.translation
        p1.scaling shouldBe p2.scaling +- 1e-14
        val r1 = Rotation3D.fromEulerXYZ(p1.pitch, p1.yaw, p1.roll)
        val r2 = Rotation3D.fromEulerXYZ(p2.pitch, p2.yaw, p2.roll)
        rotationEqual(r1, r2)
      }

      it("random pose composes with neutral pose") {
        val p1 = Pose.neutral
        val p2 = randomPose()
        poseEqual(p1 compose p2, p2)
      }

      it("rotation poses compose (yaw)") {
        val p1 = Pose.neutral.copy(yaw = 0.5)
        val p2 = Pose.neutral.copy(yaw = -0.25)
        (p1 compose p2).yaw shouldBe 0.25 +- 1e-10
      }

      it("rotation poses compose (roll)") {
        val p1 = Pose.neutral.copy(roll = 0.5)
        val p2 = Pose.neutral.copy(roll = -0.25)
        (p1 compose p2).roll shouldBe 0.25 +- 1e-10
      }

      it("rotation poses compose (pitch)") {
        val p1 = Pose.neutral.copy(pitch = 0.5)
        val p2 = Pose.neutral.copy(pitch = -0.25)
        (p1 compose p2).pitch shouldBe 0.25 +- 1e-10
      }

      it("rotation poses compose (all)") {
        val p1 = randomPose()
        val p2 = randomPose()
        val composed = p1 compose p2

        // check with direct rotation composition
        val r1 = Rotation3D.fromEulerXYZ(p1.pitch, p1.yaw, p1.roll)
        val r2 = Rotation3D.fromEulerXYZ(p2.pitch, p2.yaw, p2.roll)
        val rotationComposed = r1 compose r2
        rotationEqual(Rotation3D.fromEulerXYZ(composed.pitch, composed.yaw, composed.roll), rotationComposed)
      }

      it("flattens the scene tree with the right transformations") {
        val so = SceneObject(MeshVertexColor(randomGridMesh()))

        val px = Pose.neutral.copy(translation = Vector3D.unitX)
        val py = Pose.neutral.copy(translation = Vector3D.unitY)
        val pz = Pose.neutral.copy(translation = Vector3D.unitZ)

        val targetObjects: IndexedSeq[(Transform3D, RenderObject)] = IndexedSeq(
          (px.transform compose py.transform compose pz.transform, so.renderObject),
          (px.transform compose py.transform, so.renderObject),
          (px.transform, so.renderObject)
        )

        val sp: SceneTree =
          PoseNode(pose = px, children = IndexedSeq(
            PoseNode(pose = py, children = IndexedSeq(
              PoseNode(pose = pz, children = IndexedSeq(
                so)),
              so)),
            so))

        val posedObjects = sp.posedObjects

        for{
          ((pose, renderObject), (targetPose, targetObject)) <- posedObjects.zip(targetObjects)
        }{
          checkTrafoEqual(pose, targetPose)
          renderObject shouldBe targetObject
        }
      }
    }
  }

  describe("RenderParameters JSON Formats V4") {
    import RenderParameterJSONFormat.version4._

    describe("MoMoInstance JSON format") {
      writeReadTest(MoMoInstance(
        shape = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
        color = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
        expression = IndexedSeq.empty,
        modelURI = randomURI(10)))
    }
  }

  describe("Render Parameter JSON Reader") {

    import RenderParameterJSONFormat.defaultFormat._

    it("default format is V4.0") {
      RenderParameterJSONFormat.defaultFormat shouldBe RenderParameterJSONFormatV4
    }

    it("reader can read spray json (versioned dispatcher)") {
      val jstr = randomParam.toJson
      val pRead = jstr.convertTo[RenderParameter]
      identical(pRead, randomParam)
    }

    // explicit json to parse with target
    val targetParam = RenderParameter(
      view = ViewParameter(
        translation = Vector(10.0, 20.0, 2000.0),
        pitch = 0.75,
        roll = -0.125,
        yaw = 1.5),
      camera = Camera(
        far = 100000.0,
        focalLength = 50.0,
        near = 1.0,
        orthographic = false,
        principalPoint = Point(0.005555555555555556, -0.016666666666666666),
        sensorSize = Vector(36.0, 24.0)),
      colorTransform = ColorTransform(RGB(1.0, 0.5, 0.25), 1.0, RGB(0.25, 0.125, 0.75)),
      environmentMap = SphericalHarmonicsLight(IndexedSeq(Vector(0.5, 0.25, 0.125), Vector(0.125, 0.25, 0.5), Vector(0.25, 0.5, 0.125), Vector(0.5, 0.125, 0.25))),
      directionalLight = DirectionalLight(RGB.Black, RGB.Black, Vector3D.unitZ, RGB.Black, 10.0),
      imageSize = ImageSize(width = 720, height = 480),
      momo = MoMoInstance(IndexedSeq(-1.0, 0.5, -0.25), IndexedSeq(1.0, 0.0, 0.5), IndexedSeq(), new URI("modelPath1")),
      pose = Pose(1.0, Vector(0.0, 0.0, -1000.0), roll = 0.125, yaw = -0.125, pitch = 0.25))

    def readResourceAsString(resource: String): String = {
      Source.fromURL(this.getClass.getResource("/" + resource)).toString()
    }

    it("can be written to and read from a stream") {
      val os = new ByteArrayOutputStream()
      RenderParameterIO.writeToStream(randomParam, os).get
      val is = new ByteArrayInputStream(os.toByteArray)
      val readParam = RenderParameterIO.readFromStream(is).get
      identical(readParam, randomParam)
    }

    it("can read old C++ json") {
      import RenderParameterJSONFormat.version1._
      val cxxParam = Source.fromURL(this.getClass.getResource("/renderParametersV1.rps")).mkString.parseJson.convertTo[RenderParameter]
      cxxParam shouldBe targetParam
    }

    it("can read V2 example json") {
      import RenderParameterJSONFormat.version2._
      val v2Param = Source.fromURL(this.getClass.getResource("/renderParametersV2.rps")).mkString.parseJson.convertTo[RenderParameter]
      v2Param shouldBe targetParam
    }

    it("can read V3 example json") {
      import RenderParameterJSONFormat.version3._
      val v3Param = Source.fromURL(this.getClass.getResource("/renderParametersV3.rps")).mkString.parseJson.convertTo[RenderParameter]
      v3Param shouldBe targetParam
    }

    it("can read V4 example json") {
      import RenderParameterJSONFormat.version4._
      val v4Param = Source.fromURL(this.getClass.getResource("/renderParametersV4.rps")).mkString.parseJson.convertTo[RenderParameter]
      v4Param shouldBe targetParam
    }

    it("can read the correct version from the files content") {
      Seq(
        "/renderParametersV4.rps",
        "/renderParametersV3.rps",
        "/renderParametersV2.rps",
        "/renderParametersV1.rps"
      ) foreach { filename =>
        val param = RenderParameterIO.read(new File(this.getClass.getResource(filename).getPath)).get
        identical(param,targetParam)
      }
    }


    it("can be written to and read from a file") {
      val f = File.createTempFile("ParameterIOTest", ".rps")
      f.deleteOnExit()
      RenderParameterIO.write(randomParam, f).get
      val readParam = RenderParameterIO.read(f).get
      identical(readParam, randomParam)
    }

    it("can be written to and read from a file at a certain path") {
      val path = IndexedSeq.fill(4){rnd.scalaRandom.alphanumeric.take(6).mkString("")}.mkString("/")
      val f = File.createTempFile("ParameterIOTest", ".rps")
      f.deleteOnExit()
      RenderParameterIO.writeWithPath(randomParam, f, path).get
      val readParam = RenderParameterIO.readWithPath[RenderParameter](f, path).get
      identical(readParam, randomParam)
    }

  }
}