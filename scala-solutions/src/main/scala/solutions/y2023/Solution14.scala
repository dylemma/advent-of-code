package io.dylemma.aoc
package solutions.y2023

import scala.annotation.tailrec

object Solution14 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {
		val platform = Platform(input.linesIterator.toVector)

		val part1Result = platform.rollNorth
		log.debug(s"After rolling north:\n${ part1Result.rendered }")
		log.info(s"Part 1 score: ${ part1Result.score }")

		val part2Result = platform.cycleN(1000000000L)
		log.debug(s"After 1,000,000,000 cycles:\n${ part2Result.rendered }")
		log.info(s"Part 2 score: ${ part2Result.score }")

	}

	case class Position(col: Int, row: Int)

	// perform the "roll" operation by modeling the column as a string,
	// and sorting each '#'-delimited segment so that 'O' appear before '.'
	def rollTowardsHead(tiles: String): String = {
		tiles.split('#').view.map { segment =>
			val (rocks, grounds) = segment.partition(_ == 'O')
			rocks ++ grounds
			// note that `.split('#')` ends up dropping any trailing '#'
			// characters, so we put them back with `.padTo`, below
		}.mkString("#").padTo(tiles.length, '#')
	}

	case class Platform(lines: Vector[String]) extends AnyVal {
		def rendered: String = lines.mkString("\n")

		def rollWest: Platform = {
			// rollTowardsHead is naturally modeled as the rocks rolling left (west),
			// so no transformation is needed
			Platform(lines.map(rollTowardsHead))
		}

		def rollEast: Platform = {
			// to roll east, we reverse each line, roll left, then reverse again
			Platform(lines.map(line => rollTowardsHead(line.reverse).reverse))
		}

		def rollNorth: Platform = {
			// to roll north, we transpose the platform, where each column
			// is treated as a line that can be rolled to its relative left
			val columns = (0 until lines.head.length).map { colIndex =>
				rollTowardsHead(lines.view.map(_ charAt colIndex).mkString)
			}
			// ...then we un-translate the resulting columns back into rows
			Platform(
				lines.indices.view.map { lineIndex =>
					columns.view.map(_ charAt lineIndex).mkString
				}.toVector,
			)
		}

		def rollSouth: Platform = {
			// to roll south, we transpose the platform with reversed lines,
			// roll left, then un-transpose with reversed lines again
			val columns = (0 until lines.head.length).map { colIndex =>
				rollTowardsHead(
					lines.view.map(_ charAt colIndex).mkString.reverse,
				).reverse
			}
			Platform(
				lines.indices.view.map { lineIndex =>
					columns.view.map(_ charAt lineIndex).mkString
				}.toVector,
			)
		}
		
		def cycled = rollNorth.rollWest.rollSouth.rollEast
		
		def score = {
			lines
				.iterator
				.zipWithIndex
				.map { case (row, rowIndex) =>
					val rowNum = lines.length - rowIndex
					val numRocks = row.count(_ == 'O')
					rowNum.toLong * numRocks.toLong
				}
				.sum
		}

		def cycleN(n: Long): Platform = {
			// Repeatedly calling `.cycled` should eventually result in a repeating pattern.
			// Find that pattern by storing seen platforms in a Map, noting the cycle count
			// where they were first seen. Continuing the `.cycled` iteration, when we see
			// a previously-seen platform, we infer the pattern length, which allows us to
			// skip a multiple of that pattern length of the remainder of the cycles, only
			// actually performing the `(n - countWhenRepeated) % patternLength` cycles.
			val seen = collection.mutable.Map.empty[Platform, Int]

			@tailrec
			def findPattern(current: Platform, counter: Int): Either[(Platform, Int), Platform] = {
				if (counter == n) {
					// we reached the desired cycle count without finding a pattern
					Right(current)
				} else if (seen contains current) {
					// this is the second time seeing the current platform, meaning we found a pattern
					Left(current -> counter)
				} else {
					// no result found yet; continue iterating
					seen(current) = counter
					findPattern(current.cycled, counter + 1)
				}
			}

			findPattern(this, 0) match {
				case Right(finalPlatform) =>
					// no pattern found; return the final platform after `n` cycles
					finalPlatform
				case Left((repeated, countAtRepeat)) =>
					val firstSeenAt = seen(repeated)
					val patternLength = countAtRepeat - firstSeenAt
					val remainingCycles = n - countAtRepeat
					val cyclesToDo = remainingCycles % patternLength
					val cyclesToSkip = remainingCycles - cyclesToDo
					log.debug(s"Pattern of length $patternLength found after $countAtRepeat cycles (first seen at $firstSeenAt); " +
						s"skipping $cyclesToSkip cycles, then doing $cyclesToDo more")
					// now, do the remaining cycles one-by-one
					(0L until cyclesToDo).foldLeft(repeated) { (plat, _) => plat.cycled }
			}
		}
	}

}
