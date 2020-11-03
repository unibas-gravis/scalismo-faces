val scalismoPlatform = {
  val env = System.getenv("SCALISMO_PLATFORM")
  if (env != null) env else "all"
}

lazy val root = (project in file("."))
  .settings(
    organization  := "ch.unibas.cs.gravis",
    name := """scalismo-faces""",
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion  := "2.13.3",
    crossScalaVersions := Seq("2.13.3", "2.12.11"),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-opt:l:method", "-target:jvm-1.8"),
    resolvers += Resolver.jcenterRepo,
    libraryDependencies  ++= Seq(
      "ch.unibas.cs.gravis" %% "scalismo" % "0.90-RC1",
      "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.0",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    ),
    unmanagedSourceDirectories in Compile += {
      val sourceDir = (sourceDirectory in Compile).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
        case _                       => sourceDir / "scala-2.13-"
      }
    }
  )
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
    buildInfoPackage := "scalismo.faces"
  )
  .enablePlugins(GitVersioning)
  .settings(
    git.baseVersion := "develop",
    useJGit
  )
  .enablePlugins(GitBranchPrompt)
