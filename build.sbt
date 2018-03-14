import sbt.librarymanagement.{Developer, ScmInfo}

name := "timeseries"

organization := "io.github.carldata"

version := "0.6.2"

scalaVersion := "2.12.3"


publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/carldata/timeseries"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/carldata/timeseries"),
    "scm:git@github.com:carldata/timeseries.git"
  )
)

developers := List(
  Developer(
    id    = "klangner",
    name  = "Krzysztof Langner",
    email = "klangner@gmail.com",
    url   = url("http://github/klangner")
  )
)

useGpg := true

pomIncludeRepository := { _ => false }


lazy val root = project
  .in(file(".")).
  aggregate(tsJS, tsJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val ts = crossProject.crossType(CrossType.Full).in(file(".")).
  settings(
    name := "timeseries",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter-core" % "0.8.2",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"

  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.3"
  )


lazy val tsJVM = ts.jvm
lazy val tsJS = ts.js
