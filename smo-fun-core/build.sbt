name := "res-alg-core"

import sbt._
import Keys._
import com.nitro.build._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings

addCompilerPlugin(scalaMacros)

libraryDependencies ++= 
  miscDeps   ++
  mathMlDeps ++
  boofcvDeps ++
  nitroDeps  ++
  testDeps

//
// test, runtime settings
//
fork in run               := true
fork in Test              := true
parallelExecution in Test := true

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

//
// BELOW: Old, no-longer-used java-cpp stuff. Kept as reference for the moment.
// 

// lazy val os = com.nitro.build.Sys.os.name

// lazy val javacppV   = "1.2"
// lazy val tesseractV = s"3.04.01-$javacppV" 
// lazy val leptonicaV = s"1.73-$javacppV"

// lazy val tesseractDeps =
//   Seq(
//     "org.bytedeco.javacpp-presets" % "tesseract" % tesseractV,
//     "org.bytedeco.javacpp-presets" % "tesseract" % tesseractV classifier os
//   )

// lazy val leptonicaDeps = 
//   Seq(
//     "org.bytedeco.javacpp-presets" % "leptonica" % leptonicaV,
//     "org.bytedeco.javacpp-presets" % "leptonica" % leptonicaV classifier os
//   )

// // ABSOLUTELY CRITICAL FOR ANY java-cpp STUFF TO WORK !!!
// classpathTypes += "maven-plugin"
// // DO NOT REMOVE THE ABOVE LINE !!!!!!!!!!!!!!!!!!!!!!!!!

// libraryDependencies ++= 
//   tesseractDeps ++ 
//   leptonicaDeps :+ 
//   "org.bytedeco" % "javacpp" % javacppV

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////