scalaVersion in ThisBuild := "2.11.12"

lazy val noschema = (project in file("."))
  .settings(
    name := "noschema",
    version := "0.0.1",
    organization := "org.datacrafts",
    crossScalaVersions := Seq("2.11.12", "2.12.6"),
    // enabling coverage will add extra instrumentation byte code,
    // once enabled in build.sbt, it cannot be turned off from other places.
    // use sbt command to control coverage on/off, never do it here !
    // coverageEnabled := true, // THIS IS WRONG
    coverageMinimum := 80,
    coverageFailOnMinimum := true,
    coverageHighlighting := true,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-unused"
    ),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
    )
  )
