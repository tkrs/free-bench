name := "free-bench"
version := "0.1"
scalaVersion := "2.12.12"

scalacOptions ++= Seq(
        "-Ypartial-unification",
        "-Ywarn-value-discard",
)

Compile / sourceGenerators += (Compile / sourceManaged).map(Algebras.gen).taskValue

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "2.2.0",
  "io.frees" %% "iota-core" % "0.3.10",
  "org.atnos" %% "eff" % "5.11.0",
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
)

enablePlugins(JmhPlugin)
