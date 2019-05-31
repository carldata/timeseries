organization := "io.github.carldata"

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{ crossProject, CrossType }



lazy val timeseries =  (crossProject(JSPlatform, JVMPlatform) in file("."))
  .settings(
    name := "timeseries",
    organization := "io.github.carldata",
    version := "0.6.7",
    scalaVersion := "2.11.12",
    autoCompilerPlugins := true,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    ),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://github.com/carldata/timeseries")),
    pomIncludeRepository := { _ => false },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo := {
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
  ).jvmSettings (
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter-core" % "0.8.2"
    )
  ).jsSettings (
    libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.3"
  )

lazy val timeseriesJVM = timeseries.jvm
lazy val timeseriesJS = timeseries.js