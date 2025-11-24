package io.dylemma.aoc

import Utils.AsInt
import io.dylemma.aoc

import java.nio.file.{ Files, Paths }
import java.time.{ Instant, OffsetDateTime }

import ch.qos.logback.classic.Level

object Main extends Logging {

	def main(args: Array[String]): Unit = {

		val (rawCliArgs, puzzleArgs) = args.span(_ != "--")

		for (cliArgs <- CliArgs.parse(rawCliArgs)) {
			// reconfigure logging if the CLI requested debug logging
			if (cliArgs.isDebugEnabled) {
				Logging.setRootLevel(Level.DEBUG)
			}

			// CLI allows specifying year and day, but when omitted, we will try
			// to use the most recently used year/day from a local cache file.
			val maybeYearDay = CliCache.readAndUpdate { stored =>
				val year = cliArgs.year.getOrElse {
					stored.map(_._1) match {
						case Some(year) =>
							log.info(s"Year unspecified; using last-used year from cache: $year")
							year
						case None =>
							log.info("Year unspecified; using current year")
							OffsetDateTime.now.getYear
					}
				}

				val dayOpt = cliArgs.day.orElse {
					stored.map(_._2) match {
						case Some(day) =>
							log.info(s"Day unspecified; using last-used day from cache: $day")
							Some(day)
						case None =>
							log.error("Day must be specified on first run (no cache). Use --help for usage.")
							None
					}
				}

				dayOpt.map(year -> _)
			}

			val (year, day) = maybeYearDay match {
				case Some((y, d)) => y -> d
				case None => sys.exit(1)
			}

			val input =
				if (cliArgs.isExample) Inputs.getExampleInput(year, day)
				else Inputs.getInput(year, day)
			
			log.info(s"Solving year $year, day $day ...")

			// Use reflection to get an instance of the `Solution` for the given year+day
			val solutionClassName = f"io.dylemma.aoc.solutions.y$year.Solution$day%02d$$"
			val solutionClass = {
				try getClass.getClassLoader.loadClass(solutionClassName)
				catch {
					case e: ClassNotFoundException =>
						log.error(s"No solution implemented for year $year day $day")
						sys.exit(2)
				}
			}
			val solution = Option(solutionClass.getField("MODULE$"))
				.getOrElse {
					log.error(s"Solution class $solutionClassName is not a singleton object")
					sys.exit(3)
				}
				.get(null)
				.asInstanceOf[Solution]

			

			solution.run(input, puzzleArgs.toList)
		}


	}

	private object CliCache {
		private val path = Paths.get(".cli-cache").toAbsolutePath.normalize

		def lastModified = if Files.exists(path) then Files.getLastModifiedTime(path).toInstant else Instant.ofEpochMilli(0)
		def read = Option.when(Files isReadable path) { Files readString path }
		def write(content: String): Unit = {
			Files.createDirectories(path.getParent)
			Files.writeString(path, content)
		}
		def clean(): Unit = Files.deleteIfExists(path)

		def readAndUpdate(f: Option[(Int, Int)] => Option[(Int, Int)]) = {
			val oneDayAgo = Instant.now.minusSeconds(3600 * 24) // 24 hours ago
			val current: Option[(Int, Int)] =
				if (lastModified isBefore oneDayAgo) None // treat cache as expired
				else read.flatMap: contents =>
					contents.split(" ", 2) match {
						case Array(AsInt(year), AsInt(day)) => Some(year -> day)
						case _ => None
					}

			// delete the file if it was invalid or expired
			if current.isEmpty then clean()

			val updated = f(current)
			updated match {
				case `current` => // no change;
				case Some((year, day)) => write(s"$year $day")
				case None => clean()
			}
			updated
		}
	}
}
