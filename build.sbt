name := """scalismo-faces"""
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
scmInfo := Some(ScmInfo(url("https://github.com/unibas-gravis/scalismo-faces"), "git@github.com:unibas-gravis/scalismo-faces.git"))
homepage := Some(url("https://github.com/unibas-gravis/scalismo-faces"))

organization  := "ch.unibas.cs.gravis"
developers := List(Developer(
    id = "Scalismo-Faces",
    name = "Scalismo-Faces Community",
    email = "scalismo-faces@googlegroups.com",
    url = url("https://github.com/unibas-gravis/scalismo-faces/")))

scalaVersion  := "2.12.8"

crossScalaVersions := Seq("2.12.8", "2.11.8")

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2,  11)) =>  Seq("-deprecation", "-unchecked", "-feature")
    case _ => Seq("-deprecation", "-unchecked", "-feature", "-opt:l:method", "-target:jvm-1.8")
})

publishMavenStyle := true
publishTo := Some(
    if(isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
    else
        Opts.resolver.sonatypeStaging
)

libraryDependencies  ++= Seq(
    "ch.unibas.cs.gravis" %% "scalismo" % "0.17.2",
    "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.1",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// Git versioning
enablePlugins(GitVersioning, GitBranchPrompt)
com.typesafe.sbt.SbtGit.useJGit 
git.baseVersion := "develop"
