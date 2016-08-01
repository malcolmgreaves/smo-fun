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

  // // // // // // // // // //
  // //     Publishing    // //
  // // // // // // // // // //

  case class RepoInfo(
    group: String,
    name: String
  )

  lazy val doPublish= (ri: RepoInfo) => Seq(
    publishMavenStyle       := true,
    isSnapshot              := false,
    publishArtifact in Test := false,
    publishTo               := Some("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"),
    pomIncludeRepository    := { _ => false },
    pomExtra                := {
      <url>https://github.com/{ ri.group }/{ ri.name }</url>
        <licenses>
          <license>
            <name>Apache 2.0</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0</url>
            <distribution>Yes</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:{ ri.group }/{ ri.name }.git</url>
          <connection>scm:git@github.com:{ ri.group }/{ ri.name }.git</connection>
        </scm>
        <developers>
          <developer>
            <id>malcolmgreaves</id>
            <name>Malcolm W. Greaves</name>
            <email>greaves.malcolm@gmail.com</email>
            <url>https://malcolmgreaves.io/</url>
          </developer>
        </developers>
    },
    publishArtifact         := true
  )

  lazy val noPublish = Seq(
    isSnapshot              := true,
    publishArtifact in Test := false,
    publishTo               := None,
    pomIncludeRepository    := { _ => false },
    pomExtra                := { <nothing></nothing> },
    publishLocal            := {},
    publish                 := {},
    publishArtifact         := false
  )

}
