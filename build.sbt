name := "free-bench"
version := "0.1"
scalaVersion := "2.13.10"

scalafmtOnCompile := true

Compile / sourceGenerators += (Compile / sourceManaged)
  .map(Algebras.gen)
  .taskValue

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "2.8.0",
  "org.atnos" %% "eff" % "6.0.2",
  compilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full
  )
)

enablePlugins(JmhPlugin)
