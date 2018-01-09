import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}

name := "syaml"

val settings = Common.settings ++ Common.publish ++ Seq(
  organization := "org.mule.syaml",
  name := "syaml",
  version := "0.0.12-SNAPSHOT",

  libraryDependencies ++= Seq(
    "org.mule.common" %%% "scala-common" % "0.1.2",
    "org.scalatest" %%% "scalatest" % "3.0.0" % Test
  ),

  resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),

  credentials ++= Common.credentials()
)

lazy val root = project.in(file(".")).aggregate(syamlJS, syamlJVM)

lazy val syaml = crossProject
  .in(file("."))
  .settings(settings: _*)
  .jvmSettings(
    // JVM-specific settings here
  )
  .jsSettings(
    // JS-specific settings here
      scalaJSModuleKind := ModuleKind.CommonJSModule
  )

lazy val syamlJVM = syaml.jvm
lazy val syamlJS = syaml.js
