import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers, scalacOptions}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

name := "syaml"

val settings = Common.settings ++ Common.publish ++ Seq(
  organization := "org.mule.syaml",
  name := "syaml",
  version := {
    val major = 1
    val minor = 0

    lazy val build  = sys.env.getOrElse("BUILD_NUMBER", "0")
    lazy val branch = sys.env.get("BRANCH_NAME")

    if (branch.contains("master")) s"$major.$minor.$build" else s"$major.${minor + 1}.0-SNAPSHOT"
  },
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.0" % Test
  ),
  resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),
  credentials ++= Common.credentials()
)

lazy val workspaceDirectory: File =
  sys.props.get("sbt.mulesoft") match {
    case Some(x) => file(x)
    case _       => Path.userHome / "mulesoft"
  }

val scalaCommonVersion = "0.5.69"

lazy val scalaCommonJVMRef = ProjectRef(workspaceDirectory / "scala-common", "commonJVM")
lazy val scalaCommonJSRef  = ProjectRef(workspaceDirectory / "scala-common", "commonJS")
lazy val scalaCommonLibJVM = "org.mule.common" %% "scala-common" % scalaCommonVersion
lazy val scalaCommonLibJS  = "org.mule.common" %% "scala-common_sjs0.6" % scalaCommonVersion

lazy val syaml = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(settings: _*)
  .jvmSettings(
    // JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  )
  .jsSettings(
    // JS-specific settings here
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    scalacOptions += "-P:scalajs:suppressExportDeprecations"
  )

lazy val syamlJVM = syaml.jvm.in(file("./jvm")).sourceDependency(scalaCommonJVMRef, scalaCommonLibJVM)
lazy val syamlJS  = syaml.js.in(file("./js")).sourceDependency(scalaCommonJSRef, scalaCommonLibJS)
