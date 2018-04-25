import Dependencies._

lazy val compilerSettings = CompilerSettings.options ++ Seq(
  crossScalaVersions := Seq("2.11.12", scalaVersion.value)
)

lazy val root = (project in file(".")).
  settings(
    name := "Casper for RChain",
    version := "0.1",
    scalaVersion := "2.12.4"
  )
  .settings(
    libraryDependencies ++= Seq(
      scalapbRuntime,
      catsCore,
      catsMtl,
      monix
    ),
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value
    )
  )

//Gephi 0.9.2 in lib/