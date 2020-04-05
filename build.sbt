organization := "io.github.carldata"

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released

import sbtcrossproject.{crossProject, CrossType}

lazy val root = project.in(file(".")).
  aggregate(timeseriesJS, timeseriesJVM).
  settings(
    publishArtifact := false,
    publish := {},
    publishLocal := {}
  )

lazy val timeseries = (crossProject(JSPlatform, JVMPlatform) in file("."))
  .settings(
    name := "timeseries",
    organization := "io.github.carldata",
    version := "0.7.0",
    crossScalaVersions := List("2.11.12", "2.12.11", "2.13.1"),
    autoCompilerPlugins := true,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.0-M1" % "test"
    ),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://github.com/carldata/timeseries")),
    pomIncludeRepository := { _ => false },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo in ThisBuild := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    useGpg := true,
    pomExtra := <scm>
      <url>https://github.com/carldata/timeseries</url>
      <connection>scm:git@github.com:carldata/timeseries.git</connection>
    </scm>
      <developers>
        <developer>
          <id>klangner</id>
          <name>Krzysztof Langner</name>
          <url>http://github/klangner</url>
        </developer>
      </developers>
  ).jvmSettings(
  libraryDependencies ++= Seq(
    "com.storm-enroute" %% "scalameter-core" % "0.19"
  )
).jsSettings(
  libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "1.0.0"
)
updateOptions := updateOptions.value.withGigahorse(false)

publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)

lazy val timeseriesJVM = timeseries.jvm
lazy val timeseriesJS = timeseries.js