organization  := "ch.unibas.cs.gravis"
name := """scalismo-faces"""
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")) 

scalaVersion  := "2.11.7"
scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += Resolver.jcenterRepo

libraryDependencies  ++= Seq(
    "ch.unibas.cs.gravis" %% "scalismo" % "0.14.0",
    "ch.unibas.cs.gravis" % "scalismo-native-all" % "3.0.0",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// hack to resolve issues with 3.0.+ dependency of scalismo-0.14.0
// should be fixed by setting to explicit version number
dependencyOverrides += "ch.unibas.cs.gravis" % "scalismo-native-stub" % "3.0.0"

// Git versioning
enablePlugins(GitVersioning, GitBranchPrompt)
com.typesafe.sbt.SbtGit.useJGit 
git.baseVersion := "develop"
