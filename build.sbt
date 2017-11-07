name := "syaml"

val settings = Common.settings ++ Seq(
  name := "syaml",
  version := "0.0.6-SNAPSHOT",

  libraryDependencies ++= Seq(
    "org.scalactic" %%% "scalactic" % "3.0.1",
    "org.scalatest" %%% "scalatest" % "3.0.0" % Test
  ),

  Common.publish,

  credentials ++= Common.credentials()
)

lazy val root = project.in(file(".")).aggregate(syamlJS, syamlJVM)

lazy val syaml = crossProject
  .in(file("."))
  .settings(
    settings: _*
  )
  .jvmSettings(
    // JVM-specific settings here
  )
  .jsSettings(
    // JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.2"
  )

lazy val syamlJVM = syaml.jvm
lazy val syamlJS = syaml.js
