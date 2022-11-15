name := "free-bench"
version := "0.1"
scalaVersion := "3.2.1"

scalafmtOnCompile := true

scalacOptions += "-Ykind-projector:underscores"

Compile / sourceGenerators += (Compile / sourceManaged)
  .map(Algebras.gen)
  .taskValue

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "2.8.0",
  "org.atnos" %% "eff" % "6.0.2"
)

enablePlugins(JmhPlugin)
