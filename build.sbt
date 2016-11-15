organization  := "ch.unibas.cs.gravis"

name := """scalismo-faces"""

version := "0.1"

scalaVersion  := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += "Statismo (public)" at "http://shapemodelling.cs.unibas.ch/repository/public"

resolvers += Opts.resolver.sonatypeSnapshots

publishTo := Some(Resolver.file("file", new File("/export/contrib/statismo/repo/private")))

libraryDependencies  ++= Seq(
    "ch.unibas.cs.gravis" %% "scalismo" % "0.14.+"
)

