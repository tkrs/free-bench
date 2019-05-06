name := "free-bench"
version := "0.1"
scalaVersion := "2.12.8"

scalacOptions ++= Seq(
        "-Ypartial-unification",
        "-Ywarn-value-discard",
)

Compile / sourceGenerators += (sourceManaged in Compile).map(Algebras.gen).taskValue

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "1.6.0",
  "io.frees" %% "iota-core" % "0.3.10",
  "org.atnos" %% "eff" % "5.5.0",
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0")
)

enablePlugins(JmhPlugin)
