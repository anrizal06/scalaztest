name := "scalaz" + "examples"

name ~= { s => s.toUpperCase }

version := 0 + "." + 1

scalaVersion := "2.9.1"

// libraryDependencies += "org.scalaz" % "scalaz-core_2.9.1" %  "7.0-SNAPSHOT"

// libraryDependencies += "org.scalaz" % "scalaz-core_2.9.1" % "6.0.3"

libraryDependencies += "org.scalatest" % "scalatest_2.9.1" % "1.6.1" % "test"

// resolvers ++= Seq("Scala tools" at "http://scala-tools.org/release-snapshots/")

