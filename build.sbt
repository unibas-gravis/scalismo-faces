val scalismoPlatform = {
  val env = System.getenv("SCALISMO_PLATFORM")
  if (env != null) env else "all"
}

lazy val root = (project in file("."))
  .settings(
    name := """scalismo-faces""",
    organization  := "ch.unibas.cs.gravis",
    homepage := Some(url("https://github.com/unibas-gravis/scalismo-faces")),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(ScmInfo(url("https://github.com/unibas-gravis/scalismo-faces"), "git@github.com:unibas-gravis/scalismo-faces.git")),
    developers := List(Developer("Andreas-Forster","Andreas Morel-Forster", "forster.andreas@unibas.ch",url("https://github.com/Andreas-Forster"))),
    publishMavenStyle := true,
    publishTo := Some(
      if(isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    scalaVersion  := "2.13.3",
    crossScalaVersions := Seq("2.13.3", "2.12.11"),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-opt:l:method", "-target:jvm-1.8"),
    libraryDependencies  ++= Seq(
      "ch.unibas.cs.gravis" %% "scalismo" % "0.90.0",
      "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.1",
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
