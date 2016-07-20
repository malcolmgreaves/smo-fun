name := "smo-fun-core"

import sbt._
import Keys._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings

addCompilerPlugin(scalaMacros)

libraryDependencies ++= 
  miscDeps   ++
  mathMlDeps ++
  testDeps

//
// test, runtime settings
//
fork in run               := true
fork in Test              := true
parallelExecution in Test := true

