name := "fp_in_scala"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.4" % "test"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "4.3.4" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")
