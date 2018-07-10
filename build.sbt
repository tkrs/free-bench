name := "free-bench"
version := "0.1"
scalaVersion := "2.12.6"

Compile / sourceGenerators += (sourceManaged in Compile).map(Algebras.gen).taskValue

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "1.1.0",
  "io.frees" %% "iota-core" % "0.3.8",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
)

enablePlugins(JmhPlugin)
