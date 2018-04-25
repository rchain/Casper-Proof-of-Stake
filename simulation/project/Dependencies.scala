import sbt._

object Dependencies {

  // format: off
  val catsCore            = "org.typelevel"              %% "cats-core"                 % "1.0.1"
  val catsMtl             = "org.typelevel"              %% "cats-mtl-core"             % "0.2.1"
  val monix               = "io.monix"                   %% "monix"                     % "3.0.0-M3"
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.13.4" % "test"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test"
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.0.1" % "test"
  val scalapbRuntime      = "com.thesamet.scalapb"       %% "scalapb-runtime"           % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val scalapbRuntimegGrpc = "com.thesamet.scalapb"       %% "scalapb-runtime-grpc"      % scalapb.compiler.Version.scalapbVersion
  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.0.5" % "test"
  val shapeless           = "com.chuusai"                %% "shapeless"                 % "2.3.2"
  // format: on

  private val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

  private val testing = Seq(scalactic, scalatest, scalacheck)

  val protobufDependencies: Seq[ModuleID] =
    Seq(scalapbRuntime)
}
