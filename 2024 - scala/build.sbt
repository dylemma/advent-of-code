ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
	.settings(
		name := "2024 - scala",
		idePackagePrefix.withRank(KeyRanks.Invisible) := Some("io.dylemma.aoc"),
		run / fork := true,
		run / outputStrategy := Some(StdoutOutput),
		libraryDependencies ++= Seq(
			"ch.qos.logback" % "logback-classic" % "1.5.21",
			"org.slf4j" % "slf4j-api" % "2.0.17",
			"org.typelevel" %% "cats-core" % "2.13.0"
		),
	)
