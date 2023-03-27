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
    developers := List(Developer(
    	id = "Scalismo-Faces",
	name = "Scalismo-Faces Community",
	email = "scalismo-faces@googlegroups.com",
	url = url("https://github.com/unibas-gravis/scalismo-faces/"))),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    publishMavenStyle := true,
    publishTo := Some(
      if(isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    scalaVersion  := "3.2.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"), // "-opt:l:method",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    libraryDependencies  ++= Seq(
      "ch.unibas.cs.gravis" %% "scalismo" % "0.92-SNAPSHOT",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      "io.spray" %%  "spray-json" % "1.3.6"
    ),
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
