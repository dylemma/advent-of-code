import sbt._

object Dependencies {
	lazy val cats = "org.typelevel" %% "cats-core" % "2.9.0"
	lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.4.2"
	lazy val fs2 = "co.fs2" %% "fs2-core" % "3.4.0"
	lazy val fs2Io = "co.fs2" %% "fs2-io" % "3.4.0"
	lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"
	lazy val spacCore = "io.dylemma" %% "spac-core" % "0.11.0"
	lazy val spacFs2 = "io.dylemma" %% "spac-interop-fs2" % "0.11.0"
	lazy val sttp = "com.softwaremill.sttp.client3" %% "core" % "3.8.3"
	lazy val sttpCatsBackend = "com.softwaremill.sttp.client3" %% "armeria-backend-cats" % "3.8.3"
}
