organization  := "ch.unibas.cs.gravis"
name := """scalismo-faces"""
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")) 

scalaVersion  := "2.12.6"

crossScalaVersions := Seq("2.12.6", "2.11.8")

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2,  11)) =>  Seq("-deprecation", "-unchecked", "-feature")
    case _ => Seq("-deprecation", "-unchecked", "-feature", "-opt:l:method")
})

resolvers += Resolver.jcenterRepo

libraryDependencies  ++= Seq(
    "ch.unibas.cs.gravis" %% "scalismo" % "0.17-RC1",
    "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.0",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// Git versioning
enablePlugins(GitVersioning, GitBranchPrompt)
com.typesafe.sbt.SbtGit.useJGit 
git.baseVersion := "noVertexColorMesh3D"
