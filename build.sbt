name := "heap-inspector"

version := "0.1"

scalaVersion := "2.12.4"

lazy val commonSettings = Seq(
  organization := "com.omatz",
  version := "0.1",
  scalaVersion := "2.12.4"
)

lazy val jvmCoreDumpInspector = (project in file("jvm-core-dump-inspector"))
  .settings(
    commonSettings,
    libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.2"
  )

lazy val jvmCoreDumpInspectorSessions = (project in file("jvm-core-dump-inspector-sessions"))
  .settings(commonSettings)
  .dependsOn(jvmCoreDumpInspector)

