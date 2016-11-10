logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform"       % "1.3.0")
addSbtPlugin("com.gonitro"      % "avro-codegen-compiler" % "0.3.4")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")

// If you don't have this already specified under your global sbt settings,
// (under $HOME/.sbt/0.13/global.sbt),
// then comment-out the following "addSbtPlugin" line:
//addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.8.0")
