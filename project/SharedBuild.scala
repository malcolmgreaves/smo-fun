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

  lazy val scalaMacros =
    "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full

  lazy val mainDeps = Seq(
    "org.spire-math" %% "spire"       % "0.11.0",
    "org.scalaz"     %% "scalaz-core" % "7.2.4",
    "org.scalanlp"   %% "breeze"      % breezeV
  )

  lazy val cmdProjAddDeps = Seq(
    "com.quantifind"    %% "wisp"           % "0.0.4",
    "io.malcolmgreaves" %% "abstract_data"  % dataV,
    "org.scalanlp"      %% "breeze-natives" % breezeV
  )


  lazy val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )

}
