package io.dylemma.aoc
package solutions.y2023

import Utils.{ AsInt, AsTuple2 }

object Solution15 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		if (shouldRunPart1) {
			val p1Sum = input.trim.split(',').view
				.map { s => s -> hash(s) }
				.tapEach { case (s, h) => log.info(s"  '$s' -> $h") }
				.map(_._2)
				.sum
			log.info(s"Part 1: $p1Sum")
		}

		if (shouldRunPart2) {
			val p2Sum = Part2.compute(input).score
			log.info(s"Part 2: $p2Sum")
		}
	}

	def step(accum: Int, char: Char): Int = {
		((accum + char.toInt) * 17) % 256
	}

	def hash(s: String): Int = s.foldLeft(0)(step)

	object Part2 {
		case class BoxEntry(label: String, focalLength: Int) {
			override def toString = s"[$label $focalLength]"
		}

		// We reinvented a HashMap, Merry Xmas!
		case class Boxes(byHash: Map[Int, Vector[BoxEntry]]) extends AnyVal {
			def removeLabel(label: String) = Boxes {
				byHash.updatedWith(hash(label)) {
					_.map { entries =>
						entries.filterNot(_.label == label)
					}
				}
			}

			def insertLabel(label: String, focalLength: Int) = Boxes {
				val entry = BoxEntry(label, focalLength)
				byHash.updatedWith(hash(label)) {
					case None => Some(Vector(entry))
					case Some(entries) =>
						if entries.exists(_.label == label) then
							Some(entries.map {
								case BoxEntry(`label`, _) => BoxEntry(label, focalLength)
								case other => other
							})
						else
							Some(entries :+ entry)

				}
			}

			// arbitrary computation to solve the puzzle
			def score = {
				(for {
					(boxIndex, entries) <- byHash.view
					(BoxEntry(label, focalLength), position) <- entries.view.zipWithIndex
				} yield {
					val boxNum = boxIndex + 1
					val slotNum = position + 1
					val score = boxNum * slotNum * focalLength
					log.debug(s"$label: $boxNum (box $boxIndex) * $slotNum (slot $position) * $focalLength (focalLength) = $score")
					score
				}).sum
			}
		}

		def compute(input: String): Boxes = {
			input.trim.split(',').view
				.foldLeft(Boxes(Map.empty)) { (accum, s) =>
					// actual computation of next state
					val result = {
						if (s.endsWith("-")) {
							val label = s.stripSuffix("-")
							accum.removeLabel(label)

						} else if (s.contains('=')) {
							s.split('=') match {
								case AsTuple2(label, AsInt(focalLength)) =>
									accum.insertLabel(label, focalLength)
								case _ =>
									throw new IllegalArgumentException(s"Invalid box entry: '$s'")
							}

						} else {
							throw new IllegalArgumentException(s"Unexpected step: '$s'")
						}
					}

					// debug output
					log.debug(s"After '$s':")
					for ((index, entries) <- result.byHash if entries.nonEmpty) {
						log.debug(s"  Box $index: ${ entries.mkString(" ") }")
					}

					// return next state for the loop
					result
				}
		}
	}
}
