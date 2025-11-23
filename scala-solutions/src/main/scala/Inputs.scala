package io.dylemma.aoc

import Utils.AsInt

import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{ HttpRequest, HttpResponse }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }
import java.time.{ Instant, OffsetDateTime }

object Inputs extends Logging {
	val InputsFilePath = "./inputs"
	private val ExampleInputsResourcePath = "/example-inputs"
	private val SessionCookieFileName = "session.txt"
	private def inputFileName(day: Int): String = s"day$day.txt"

	/** Get or download the (real) puzzle input for the given day.
	  *
	  * Inputs are cached in the `./inputs` directory to avoid re-downloading every time.
	  *
	  * @param day The day number (1-25)
	  * @return The puzzle input as a String
	  */
	def getInput(year: Int, day: Int): String = {
		val inputPath = Paths.get(InputsFilePath, s"$year", inputFileName(day)).toAbsolutePath.normalize
		if (Files.exists(inputPath)) {
			log.info(s"Reading input from $inputPath")
			Files.readString(inputPath).trim
		} else {
			log.info(s"Downloading input for day $day...")
			val input = downloadInput(year, day)
			Files.createDirectories(inputPath.getParent)
			log.debug(s"Saving input to $inputPath")
			Files.writeString(inputPath, input)
			input.trim
		}
	}

	private def getSessionCookie: String = {
		val sessionPath = Paths.get(InputsFilePath, SessionCookieFileName).toAbsolutePath.normalize
		log.debug(s"Reading session cookie from $sessionPath")
		if (!Files.exists(sessionPath)) {
			throw new RuntimeException(s"Session cookie not found at $sessionPath")
		}
		Files.readString(sessionPath).trim
	}

	private def downloadInput(year: Int, day: Int): String = {
		val session = getSessionCookie
		val req = HttpRequest
			.newBuilder(URI.create(s"https://adventofcode.com/$year/day/$day/input"))
			.header("Cookie", s"session=$session")
			.header("User-Agent", "@dylemma AdventOfCode 2024 Scala")
			.GET
			.build

		val response: HttpResponse[String] = Http.client.send(req, BodyHandlers.ofString)
		if (response.statusCode() != 200) {
			throw new RuntimeException(s"Failed to download input: status=${ response.statusCode }")
		}
		response.body()
	}

	/** Get the (example) puzzle input for the given day.
	  *
	  * Example inputs are stored as resources in the `example-inputs` directory.
	  *
	  * @param day The day number (1-25)
	  * @return The example puzzle input as a String
	  */
	def getExampleInput(year: Int, day: Int): String = {
		getClass.getResourceAsStream(s"$ExampleInputsResourcePath/$year/${ inputFileName(day) }") match {
			case null => throw new RuntimeException(s"Example input for day $day not found")
			case stream =>
				log.info(s"Reading example input for day $day from resources")
				new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
		}
	}
	
}
