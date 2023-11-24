import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / version := getVersion(2, 0)
ThisBuild / scalaVersion := "2.12.18"

val scalaCommonVersion     = "2.1.0-JAVA21-INTERNAL"
val scalaCommonTestVersion = "0.2.0-JAVA21-INTERNAL"

val settings = Common.settings ++ Common.publish ++ Seq(
    organization := "org.mule.syaml",
    name := "syaml",
    libraryDependencies ++= Seq(
        "org.mule.common" %%% "scala-common-test" % scalaCommonTestVersion % Test
    ),
    resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),
    credentials ++= Common.credentials()
)

lazy val workspaceDirectory: File =
  sys.props.get("sbt.mulesoft") match {
    case Some(x) => file(x)
    case _       => Path.userHome / "mulesoft"
  }

lazy val scalaCommonJVMRef = ProjectRef(workspaceDirectory / "scala-common", "commonJVM")
lazy val scalaCommonJSRef  = ProjectRef(workspaceDirectory / "scala-common", "commonJS")
lazy val scalaCommonLibJVM = "org.mule.common" %% "scala-common" % scalaCommonVersion
lazy val scalaCommonLibJS  = "org.mule.common" %% "scala-common_sjs1" % scalaCommonVersion

lazy val syaml = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(settings: _*)
  .settings(AutomaticModuleName.settings("org.syaml"))
  .jvmSettings(
      // JVM-specific settings here
      libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided"
  )
  .jsSettings(
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val syamlJVM = syaml.jvm.in(file("./jvm")).sourceDependency(scalaCommonJVMRef, scalaCommonLibJVM)
lazy val syamlJS = syaml.js
  .in(file("./js"))
  .sourceDependency(scalaCommonJSRef, scalaCommonLibJS)
  .disablePlugins(SonarPlugin, ScoverageSbtPlugin)

def getVersion(major: Int, minor: Int) = {
  lazy val build  = sys.env.getOrElse("BUILD_NUMBER", "0")
  lazy val branch = sys.env.get("BRANCH_NAME")

  if (branch.contains("master")) s"$major.$minor.$build" else s"$major.${minor + 1}.0-SNAPSHOT"
}

lazy val sonarUrl   = sys.env.getOrElse("SONAR_SERVER_URL", "Not found url.")
lazy val sonarToken = sys.env.getOrElse("SONAR_SERVER_TOKEN", "Not found token.")
lazy val branch     = sys.env.getOrElse("BRANCH_NAME", "develop")

sonarProperties := Map(
    "sonar.login"             -> sonarToken,
    "sonar.projectKey"        -> "mulesoft.syaml",
    "sonar.projectName"       -> "syaml",
    "sonar.projectVersion"    -> version.value,
    "sonar.sourceEncoding"    -> "UTF-8",
    "sonar.github.repository" -> "aml-org/syaml",
    "sonar.branch.name"       -> branch,
    "sonar.sources"           -> "shared/src/main/scala",
    "sonar.tests"             -> "shared/src/test/scala",
    "sonar.userHome"          -> "${buildDir}/.sonar"
)
