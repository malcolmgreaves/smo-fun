import sbt._
import Keys._

object SharedBuild {

  // // // // // // // //
  // //   Versions  // //
  // // // // // // // //

  lazy val breezeV = "0.12"
  lazy val dataV   = "1.0.0"

  // // // // // // // // // //
  // //    Dependencies   // //
  // // // // // // // // // //

  lazy val scalaMacros = "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full

  lazy val avroCodegen = nOss          %% "avro-codegen-runtime" % "0.3.4"
  lazy val shapeless   = "com.chuusai" %% "shapeless"            % "2.2.5"

  lazy val miscDeps = Seq(
    "io.malcolmgreaves"    %% "abstract_data" % dataV,
    "io.argonaut"          %% "argonaut"      % "6.1",
    "org.scalaj"           %% "scalaj-http"   % "2.2.1",
    "com.github.mpilquist" %% "simulacrum"    % "0.7.0"  
  ) :+ shapeless

  lazy val mathMlDeps = Seq(
    "org.scalanlp"   %% "breeze"         % breezeV,
    "org.scalanlp"   %% "breeze-natives" % breezeV,
    "org.scalanlp"   %% "nak"            % "1.3",
    "com.quantifind" %% "wisp"           % "0.0.4"
  )

  lazy val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )

}
