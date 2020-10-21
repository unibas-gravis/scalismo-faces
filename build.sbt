organization  := "ch.unibas.cs.gravis"
name := """scalismo-faces"""
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")) 

scalaVersion  := "2.12.8"

crossScalaVersions := Seq("2.12.8", "2.11.8")

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2,  11)) =>  Seq("-deprecation", "-unchecked", "-feature", "-target:jvm-1.8")
    case _ => Seq("-deprecation", "-unchecked", "-feature", "-opt:l:method", "-target:jvm-1.8")
})

resolvers += Resolver.jcenterRepo

libraryDependencies  ++= Seq(
    "ch.unibas.cs.gravis" %% "scalismo" % "0.90-RC1", // this is in a public repository, but it is an experimental development version of scalismo
    "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

// Git versioning
enablePlugins(GitVersioning, GitBranchPrompt)
com.typesafe.sbt.SbtGit.useJGit 
git.baseVersion := "develop"
