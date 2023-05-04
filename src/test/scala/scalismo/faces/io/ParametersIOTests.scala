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

import java.io.*
import java.net.URI
import scalismo.faces.FacesTestSuite
import scalismo.color.{RGB, RGBA}
import scalismo.faces.image.PixelImageDomain
import scalismo.faces.io.renderparameters.fromats.{MoMoInstanceLegacyFormat, SphericalHarmonicsLegacyFormat}
import scalismo.faces.io.renderparameters.{
  RenderParameterJSONFormat,
  RenderParameterJSONFormatLegacy,
  RenderParameterJSONFormatV2,
  RenderParameterJSONFormatV4,
  RenderParameterJsonFormatV3
}
import scalismo.faces.mesh.*
import scalismo.faces.parameters.*
import scalismo.faces.render.*
import scalismo.faces.utils.ResourceManagement
import scalismo.geometry.*
import scalismo.mesh.{SurfacePointProperty, VertexColorMesh3D}

import scala.io.Source
import scala.reflect.ClassTag
import upickle.default.{read, writeJs as write}

class ParametersIOTests extends FacesTestSuite {

  private def d = rnd.scalaRandom.nextDouble()
  private def randomAngle(range: Double = 2 * math.Pi) = d * range
  private def randomList(n: Int) = IndexedSeq.fill(n)(rnd.scalaRandom.nextGaussian())

  def randomParameter(): RenderParameter = {
    val w = rnd.scalaRandom.nextInt(2048)
    val h = rnd.scalaRandom.nextInt(2048)
    val n = rnd.scalaRandom.nextInt(400)

    RenderParameter(
      pose = Pose(scaling = d * 2 + 1f,
                  translation = EuclideanVector(d * 100, d * 100, -10000 * d),
                  pitch = randomAngle(),
                  yaw = randomAngle(),
                  roll = randomAngle()
      ),
      view = ViewParameter(translation = EuclideanVector(d * 100, d * 100, d * 1000),
                           pitch = randomAngle(),
                           yaw = randomAngle(),
                           roll = randomAngle()
      ),
      camera = Camera(
        focalLength = d * 40,
        principalPoint = Point(d * 2 - 1, d * 2 - 1),
        sensorSize = EuclideanVector(d * 100, d * 100),
        near = d * 0.1,
        far = 1e12,
        orthographic = rnd.scalaRandom.nextBoolean()
      ),
      environmentMap = SphericalHarmonicsLight(IndexedSeq.fill(9)(EuclideanVector(d, d, d))),
      directionalLight = DirectionalLight.off,
      momo = MoMoInstance(shape = randomList(n), color = randomList(n), expression = randomList(n), randomURI(20)),
      imageSize = ImageSize(width = w, height = h),
      colorTransform = ColorTransform(gain = randomRGB, colorContrast = d, offset = randomRGB)
    )
  }

  // explicit json to parse with target
  val targetParam = RenderParameter(
    view = ViewParameter(translation = EuclideanVector(10.0, 20.0, 2000.0), pitch = 0.75, roll = -0.125, yaw = 1.5),
    camera = Camera(
      far = 100000.0,
      focalLength = 50.0,
      near = 1.0,
      orthographic = false,
      principalPoint = Point(0.005555555555555556, -0.016666666666666666),
      sensorSize = EuclideanVector(36.0, 24.0)
    ),
    colorTransform = ColorTransform(RGB(1.0, 0.5, 0.25), 1.0, RGB(0.25, 0.125, 0.75)),
    environmentMap = SphericalHarmonicsLight(
      IndexedSeq(EuclideanVector(0.5, 0.25, 0.125),
                 EuclideanVector(0.125, 0.25, 0.5),
                 EuclideanVector(0.25, 0.5, 0.125),
                 EuclideanVector(0.5, 0.125, 0.25)
      )
    ),
    directionalLight = DirectionalLight(RGB.Black, RGB.Black, EuclideanVector3D.unitZ, RGB.Black, 10.0),
    imageSize = ImageSize(width = 720, height = 480),
    momo = MoMoInstance(IndexedSeq(-1.0, 0.5, -0.25), IndexedSeq(1.0, 0.0, 0.5), IndexedSeq(), new URI("modelPath1")),
    pose = Pose(1.0, EuclideanVector(0.0, 0.0, -1000.0), roll = 0.125, yaw = -0.125, pitch = 0.25)
  )

  def vec2Close(v1: EuclideanVector[_2D], v2: EuclideanVector[_2D]): Unit = {
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

  var idx = 0
  def writeReadTest[A: ClassTag](a: A)(implicit format: upickle.default.ReadWriter[A]): Unit = {
    idx += 1
    val tag = implicitly[ClassTag[A]]
    it(s"consistently writes/reads ${tag.runtimeClass.getName} - ${idx}") {
      val tmp = write(a)
      read[A](tmp) shouldBe a
    }
  }

  // choose a random parameter for this test: "ensures" different values for each field
  val randomParam: RenderParameter = randomParameter()

  describe("RenderParameter JSON V1 Legacy Format") {
    import scalismo.faces.io.renderparameters.RenderParameterJSONFormatLegacy._

    writeReadTest(MoMoInstanceLegacyFormat(randomList(10), new java.net.URI("foobarr"), randomList(10), randomDouble))
    writeReadTest(
      SphericalHarmonicsLegacyFormat(IndexedSeq.fill(9)(randomList(3)).map(l => EuclideanVector(l(0), l(1), l(2))))
    )

    it("can read old C++ json") {
      val source = Source
        .fromURL(this.getClass.getResource("/renderParametersV1.rps"))
      val text = source.getLines().mkString("")
      val json = ujson.read(text)
      val cxxParam = RenderParameterIO.readFromASTWithPath[RenderParameter](json).get
      cxxParam shouldBe targetParam
    }

    it("can convert to and from C++ json") {
      import RenderParameterJSONFormatLegacy.rpsMapper
      val jstr = write(targetParam)
      val pRead = read[RenderParameter](jstr)
      identical(pRead, targetParam)
    }

  }

  describe("RenderParameter JSON format V2") {
    import scalismo.faces.io.renderparameters.RenderParameterJSONFormatV2.rpsMapper

    describe("When the illumination is a directional light") {
      import RenderParameterJSONFormatV2.illuminationMapper
      writeReadTest[Illumination](
        DirectionalLight(randomRGB, randomRGB, randomVector3D, randomRGB, 42.31415).asInstanceOf[Illumination]
      )
    }
    describe("When the illumination is a spherical harmonics light") {
      import RenderParameterJSONFormatV2.illuminationMapper
      writeReadTest[Illumination](
        SphericalHarmonicsLight(IndexedSeq.fill(9)(randomList(3)).map(l => EuclideanVector(l(0), l(1), l(2))))
          .asInstanceOf[Illumination]
      )
    }

    it("can read V2 example json") {
      val source = Source
        .fromURL(this.getClass.getResource("/renderParametersV2.rps"))
      val text = source.getLines().mkString("")
      val json = ujson.read(text)
      val v2Param = RenderParameterIO.readFromASTWithPath(json).get
      v2Param shouldBe targetParam
    }
  }

  describe("RenderParameters JSON Formats V3") {
    import RenderParameterJsonFormatV3._

    it("can read V3 example json") {
      val source = Source
        .fromURL(this.getClass.getResource("/renderParametersV3.rps"))
      val text = source.getLines().mkString("")
      val json = ujson.read(text)
      val v3Param = RenderParameterIO.readFromASTWithPath[RenderParameter](json).get
      v3Param shouldBe targetParam
    }

    describe("View parameterization") {
      writeReadTest(ViewParameter(randomVector3D, randomDouble, randomDouble, randomDouble))
    }

    describe("RenderObject JSON format") {
      val mesh: VertexColorMesh3D = randomGridMesh()

      writeReadTest(MeshVertexColor(mesh))

      writeReadTest(MeshColorNormals(ColorNormalMesh3D(mesh)))

      writeReadTest(MeshFile(randomURI(10)))

      writeReadTest(
        MoMoInstance(
          shape = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
          color = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
          expression = IndexedSeq.empty,
          modelURI = randomURI(10)
        )
      )
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

        val uvPoints = mesh.shape.pointSet.points.map { pnorm }.toIndexedSeq
        SurfacePointProperty(mesh.shape.triangulation, uvPoints)
      }

      val colTex = TextureMappedProperty.fromSurfaceProperty(mesh.color,
                                                             textureMapping,
                                                             PixelImageDomain(10, 10),
                                                             RGBA(1, 1, 0, 1)
      )
      writeReadTest(colTex)

      val vcPerTriangle = VertexPropertyPerTriangle.fromPointProperty(mesh.shape.vertexNormals)
      writeReadTest(vcPerTriangle)

      val indirect = IndirectProperty(mesh.shape.triangulation,
                                      IndexedSeq.fill(mesh.shape.triangulation.triangles.length)(0),
                                      IndexedSeq(mesh.color)
      )
      writeReadTest(indirect)
    }

    describe("Illumination JSON format") {
      writeReadTest(
        DirectionalLight(randomRGB, randomRGB, randomVector3D.normalize, randomRGB, rnd.scalaRandom.nextDouble())
          .asInstanceOf[Illumination]
      )
      writeReadTest(randomParam.environmentMap.asInstanceOf[Illumination])
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
          val json = write(sp)
          wr.print(json)
        }

        val text = Source.fromFile(f).mkString
        val rsp = read[SceneParameter](ujson.read(text))
        rsp shouldBe sp
      }
    }

    describe("SceneTree") {
      def randomPose() =
        Pose(randomDouble, randomVector3D, randomAngle() - math.Pi, randomAngle() - math.Pi, randomAngle() - math.Pi)

      def checkTrafoEqual(t1: Transform3D, t2: Transform3D): Unit = {
        def d() = 2 * rnd.scalaRandom.nextDouble() - 1
        def randomPoint() = Point3D(d(), d(), d())
        for (i <- 0 until 20) {
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

        val px = Pose.neutral.copy(translation = EuclideanVector3D.unitX)
        val py = Pose.neutral.copy(translation = EuclideanVector3D.unitY)
        val pz = Pose.neutral.copy(translation = EuclideanVector3D.unitZ)

        val targetObjects: IndexedSeq[(Transform3D, RenderObject)] = IndexedSeq(
          (px.transform compose py.transform compose pz.transform, so.renderObject),
          (px.transform compose py.transform, so.renderObject),
          (px.transform, so.renderObject)
        )

        val sp: SceneTree =
          PoseNode(pose = px,
                   children = IndexedSeq(PoseNode(pose = py,
                                                  children =
                                                    IndexedSeq(PoseNode(pose = pz, children = IndexedSeq(so)), so)
                                         ),
                                         so
                   )
          )

        val posedObjects = sp.posedObjects

        for {
          ((pose, renderObject), (targetPose, targetObject)) <- posedObjects.zip(targetObjects)
        } {
          checkTrafoEqual(pose, targetPose)
          renderObject shouldBe targetObject
        }
      }
    }
  }

  describe("RenderParameters JSON Formats V4") {
    import scalismo.faces.io.renderparameters.fromats.RenderParameterJSONFormats._

    describe("MoMoInstance JSON format") {
      writeReadTest(
        MoMoInstance(
          shape = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
          color = IndexedSeq.fill(10)(rnd.scalaRandom.nextDouble()),
          expression = IndexedSeq.empty,
          modelURI = randomURI(10)
        )
      )
    }
  }

  describe("Render Parameter JSON Reader") {

    implicit val format: upickle.default.ReadWriter[RenderParameter] = RenderParameterJSONFormat.defaultFormat

    it("writes by default version 4") {
      RenderParameterJSONFormat.defaultFormat shouldBe RenderParameterJSONFormatV4.rpsMapper
    }

    it("can be written to and read from a stream") {
      val os = new ByteArrayOutputStream()
      RenderParameterIO.writeToStream(randomParam, os).get
      val is = new ByteArrayInputStream(os.toByteArray)
      val readParam = RenderParameterIO.readFromStream(is).get
      identical(readParam, randomParam)
    }

    it("can read V4 example json") {
      val source = Source
        .fromURL(this.getClass.getResource("/renderParametersV4.rps"))
      val text = source.getLines().mkString("")
      val json = ujson.read(text)
      val v4Param = RenderParameterIO.readFromASTWithPath(json).get
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
        identical(param, targetParam)
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
      val path = IndexedSeq.fill(4) { rnd.scalaRandom.alphanumeric.take(6).mkString("") }.mkString("/")
      val f = File.createTempFile("ParameterIOTest", ".rps")
      f.deleteOnExit()
      RenderParameterIO.writeWithPath(randomParam, f, path).get
      val readParam = RenderParameterIO.readWithPath(f, path).get
      identical(readParam, randomParam)
    }

  }
}
