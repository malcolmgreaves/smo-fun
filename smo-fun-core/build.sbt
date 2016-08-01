name := "smo-fun"

import sbt._
import Keys._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings

addCompilerPlugin(scalaMacros)

libraryDependencies ++= 
  mainDeps ++
  testDeps

//
// test, runtime settings
//
fork in run               := true
fork in Test              := true
parallelExecution in Test := true

