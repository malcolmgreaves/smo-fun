name := "smo-fun-cmd"

import sbt._
import Keys._
import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings

libraryDependencies ++=
  cmdProjAddDeps

fork in run := false