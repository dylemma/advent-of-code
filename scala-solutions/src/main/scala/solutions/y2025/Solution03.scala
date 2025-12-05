package io.dylemma.aoc
package solutions.y2025

import scala.collection.mutable

object Solution03 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		if (shouldRunPart1) {
			val part1Sum = findJoltage(input, totalDigits = 2)
			log.info(s"Part 1 sum: $part1Sum")
		}

		if (shouldRunPart2) {
			val part2Sum = findJoltage(input, totalDigits = 12)
			log.info(s"Part 2 sum: $part2Sum")
		}

	}

	def findJoltage(input: String, totalDigits: Int) = input
		.linesIterator
		.tapEach { line => log.debug(s"Input line: '$line':") }
		.map { _.iterator.map(_.asDigit).toList }
		.tapEach { batteries => log.trace(s"  Parsed batteries: $batteries") }
		.map { batteries =>

			// (tailOfInput -> numDigitsRemaining) => bestScoreFound
			val bestScores = mutable.Map.empty[(String, Int), Long]
			var searchCount = 0

			def search(remainingDigits: List[Int], digitsToPick: Int): Long = {
				searchCount += 1
				if (digitsToPick == 0) 0L
				else if (remainingDigits.isEmpty) Long.MinValue
				else {
					val key = (remainingDigits.mkString(","), digitsToPick)
					bestScores.getOrElseUpdate(key, {
						val withThisDigit = {
							val thisDigit = remainingDigits.head
							val scoreIfPicked = search(remainingDigits.tail, digitsToPick - 1)
							if (scoreIfPicked == Long.MinValue) Long.MinValue
							else thisDigit * Math.pow(10, digitsToPick - 1).toLong + scoreIfPicked
						}
						val withoutThisDigit = search(remainingDigits.tail, digitsToPick)
						withThisDigit max withoutThisDigit
					})
				}
			}

			val result = search(batteries, totalDigits)
			log.debug(s"  searched $searchCount iterations")
			result
		}
		.tapEach { max => log.debug(s"  Max combination value: $max") }
		.sum
}
