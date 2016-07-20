name := "smo-fun"


com.typesafe.sbt.SbtScalariform.defaultScalariformSettings

scalaVersion in ThisBuild := "2.11.8"
organization in ThisBuild := "io.malcolmgreaves"
version in ThisBuild      := {
  val major: Int = 0
  val minor: Int = 0
  val patch: Int = 0
  s"$major.$minor.$patch"
}

lazy val root = project
  .in(file("."))
  .aggregate(
    `smo-fun-cmd`,
    `smo-fun-core`
  )
  .settings {
    publishArtifact := false
    publishLocal    := {}
    publish         := {}
  }

lazy val `smo-fun-core` = project
  .in(file("smo-fun-core"))
  .settings { 
    publishArtifact := true
  }

lazy val `smo-fun-cmd` = project
  .in(file("smo-fun-cmd"))
  .dependsOn(`smo-fun-core`)
  .settings {
    publishArtifact := true
  }

lazy val subprojects: Seq[ProjectReference] = root.aggregate
lazy val publishTasks = subprojects.map { publish.in }

resolvers in ThisBuild := Seq(
  // sonatype, maven central
  "Sonatype Releases"  at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",

  // bintray
  "Scalaz Bintray" at "http://dl.bintray.com/scalaz/releases",
  Resolver.bintrayRepo("mfglabs", "maven"),
  Resolver.bintrayRepo("dwhjames", "maven"),

  // etc.
  "Confluent" at "http://packages.confluent.io/maven/"
)

lazy val javaV = "1.8"
scalacOptions in ThisBuild := Seq(
  "-optimize",
  "-deprecation",
  "-feature",
  "-unchecked",
  s"-target:jvm-$javaV",
  "-encoding",
  "utf8",
  "-language:postfixOps",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-language:reflectiveCalls",
  "-Yno-adapted-args",
  "-Ywarn-value-discard",
  "-Xlint",
  "-Xfuture",
  "-Ywarn-dead-code",
  "-Xfatal-warnings" // Every warning is esclated to an error.
)
javacOptions in ThisBuild := Seq("-source", javaV, "-target", javaV)
javaOptions in ThisBuild  := Seq(
  "-server", 
  "-XX:+AggressiveOpts", 
  "-XX:+TieredCompilation",
  "-XX:CompileThreshold=100",
  "-Xmx3000M",
  "-XX:+UseG1GC"
)
