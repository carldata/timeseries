name := "examples"

version := "0.1.0"

scalaVersion := "2.12.1"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"

unmanagedJars in Compile += baseDirectory.value / "../target/scala-2.12/classes/"

