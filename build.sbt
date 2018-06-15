scalaVersion in ThisBuild := "2.11.11"

lazy val noschema = (project in file("."))
  .settings(
    name := "noschema",
    version := "0.0.1",
    organization := "org.datacrafts",
    crossScalaVersions := Seq("2.11.11", "2.12.6"),
    // enabling coverage will add extra instrumentation byte code,
    // once enabled in build.sbt, it cannot be turned off from other places.
    // use sbt command to control coverage on/off, never do it here !
    // coverageEnabled := true, // THIS IS WRONG
    coverageMinimum := 100,
    coverageFailOnMinimum := true,
    coverageHighlighting := true,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Xfatal-warnings",
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-unused"
    ),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )
