import AssemblyKeys._ // put this at the top of the file
seq(assemblySettings: _*)

name := "weather"

version := "0.1"

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
