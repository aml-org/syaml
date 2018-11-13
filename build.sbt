import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

name := "syaml"

val settings = Common.settings ++ Common.publish ++ Seq(
  organization := "org.mule.syaml",
  name := "syaml",
  version := "0.6.1",

  libraryDependencies ++= Seq(
    "org.mule.common" %%% "scala-common" % "0.4.0",
    "org.scalatest" %%% "scalatest" % "3.0.0" % Test
  ),

  resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),

  credentials ++= Common.credentials()
)

lazy val url = sys.env.getOrElse("SONAR_SERVER_URL", "Not found url.")
lazy val token = sys.env.getOrElse("SONAR_SERVER_TOKEN", "Not found token.")

lazy val root = project.in(file(".")).aggregate(syamlJS, syamlJVM)
//			.enablePlugins(SonarRunnerPlugin).settings(
//  sonarProperties := {
//    Map(
//      "sonar.host.url" -> url,
//      "sonar.login" -> token,
//      "sonar.projectKey" -> "mulesoft.syaml",
//      "sonar.projectName" -> "SYaml",
//      "sonar.github.repository" -> "mulesoft/syaml",
//      "sonar.projectVersion" -> "0.0.1",
//      "sonar.sourceEncoding" -> "UTF-8",
//      "sonar.modules" -> ".",
//      "..sonar.sources" -> "shared/src/main/scala",
//      "..sonar.exclusions" -> "shared/src/test/resources/**",
//      "..sonar.tests" -> "shared/src/test/scala",
//      "..sonar.scoverage.reportPath" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml"
//    )
//  }
//)

lazy val syaml = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(settings: _*)
  .jvmSettings(
    // JVM-specific settings here
    libraryDependencies += "org.scala-js"           %% "scalajs-stubs"          % scalaJSVersion % "provided"
  )
  .jsSettings(
    // JS-specific settings here
      scalaJSModuleKind := ModuleKind.CommonJSModule
  )

lazy val syamlJVM = syaml.jvm
lazy val syamlJS = syaml.js
