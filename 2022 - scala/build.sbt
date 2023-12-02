import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
	.settings(
		name := "AdventOfCode",
		libraryDependencies += scalaTest % Test,
		libraryDependencies ++= Seq(
			cats,
			catsEffect,
			fs2,
			fs2Io,
			spacCore,
			spacFs2,
			sttp,
			sttpCatsBackend
		)
	)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
