package io.dylemma.aoc
package solutions.y2025

import scala.annotation.tailrec

import cats.kernel.Semigroup
import cats.syntax.all

object Solution07 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		if (shouldRunPart1) {
			val (endBeams, numSplits) = countSplits(Set.empty, input.linesIterator, 0)
			log.debug(s"End state of beams: $endBeams")
			log.info(s"Part 1: $numSplits")
		}

		if (shouldRunPart2) {
			val numTimelines = countTimelines(input)
			log.info(s"Part 2: $numTimelines")
		}

	}

	@tailrec
	def countSplits(beams: Set[Int], lines: Iterator[String], accumCount: Int): (Set[Int], Int) = {
		if (!lines.hasNext) {
			(beams, accumCount)
		}
		else {
			val line = lines.next()

			val newBeams = line.iterator.zipWithIndex.flatMap {
				case ('S', i) => Seq(i)
				case ('^', i) if beams.contains(i) => Seq(i - 1, i + 1)
				case ('.', i) if beams.contains(i) => Seq(i)
				case _ => Seq.empty
			}.toSet

			val numSplits = line.iterator.zipWithIndex.count {
				case ('^', i) => beams.contains(i)
				case _ => false
			}

			val timelineFactor =
				if (numSplits == 0) 1
				else numSplits

			countSplits(newBeams, lines, accumCount + numSplits)
		}
	}

	def countTimelines(input: String): Long = {
		input
			.linesIterator
			.foldLeft(Map.empty[Int, Long]) { (accum, line) =>
				// accum = Map[index -> numTimelinesLeadingToThisIndex]
				// Take advantage of Cats's Semigroup for Map to combine counts,
				// e.g. `Map(1 -> 2L) |+| Map(1 -> 3L, 2 -> 4L) == Map(1 -> 5L, 2 -> 4L)`
				Semigroup[Map[Int, Long]].combineAllOption {
					line.iterator.zipWithIndex.map {
						case ('S', i) =>
							// 'S' is an initializer; always contributes one timeline to the next row
							Map(i -> 1)
						case ('^', i) if accum.contains(i) =>
							// see how many timelines led to this splitter,
							// and contribute those same timelines to the
							// (i+1) and (i-1) positions in the next row
							val numTimelinesLeadingHere = accum(i)
							Map(
								(i - 1) -> numTimelinesLeadingHere,
								(i + 1) -> numTimelinesLeadingHere
							)
						case ('.', i) if accum.contains(i) =>
							// if any timelines led to this position, carry them forward
							Map(i -> accum(i))
						case _ => Map.empty
					}
				}.getOrElse(Map.empty)
			}
			.iterator
			.tapEach { case (index, count) =>
				log.debug(s"Got $count timelines leading to idx:$index")
			}
			.map(_._2)
			.sum
	}


}
