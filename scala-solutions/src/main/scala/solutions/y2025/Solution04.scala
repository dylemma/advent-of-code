package io.dylemma.aoc
package solutions.y2025

import scala.annotation.tailrec
import scala.collection.View

object Solution04 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		val grid = Grid(input.linesIterator.toVector)

		if (shouldRunPart1) {
			val (removedGrid, numPositions) = grid.removedAccessible
			log.debug(s"Grid after removing accessible rolls once:\n${removedGrid.render}")
			log.info(s"Part 1: Found $numPositions accessible rolls")

		}

		if (shouldRunPart2) {
			@tailrec
			def loopRemove(grid: Grid, totalRemoved: Int): (Grid, Int) = {
				val (newGrid, numRemoved) = grid.removedAccessible
				log.debug(s"Removed $numRemoved accessible rolls this pass:\n${newGrid.render}")
				if (numRemoved == 0) grid -> totalRemoved
				else loopRemove(newGrid, totalRemoved + numRemoved)
			}
			loopRemove(grid, 0) match {
				case (finalGrid, totalRemoved) =>
					log.debug(s"Final grid after no more accessible rolls can be removed:\n${finalGrid.render}")
					log.info(s"Part 2: Removed $totalRemoved total rolls")
			}
		}
	}

	case class Grid(lines: Vector[String]) {
		def height: Int = lines.length
		def width: Int = if (lines.isEmpty) 0 else lines.head.length

		def charAt(x: Int, y: Int): Option[Char] = lines.lift(y).flatMap(_.lift(x))
		def get(x: Int, y: Int): Char = lines(y)(x)

		def neighbors(x: Int, y: Int): View[(Int, Int)] = {
			for {
				dx <- (-1 to 1).view
				dy <- (-1 to 1).view
				if dy != 0 || dx != 0
			} yield (x + dx, y + dy)
		}

		def accessible(x: Int, y: Int): Boolean = {
			neighbors(x, y).count { case (nx, ny) =>
				charAt(nx, ny).contains('@')
			} < 4
		}

		def render = lines.mkString("\n")

		def removedAccessible: (Grid, Int) = {
			lines.zipWithIndex.map { case (line, y) =>
				val updated = line.zipWithIndex.map {
					case ('@', x) if accessible(x, y) => '.' -> 1
					case (c, _) => c -> 0
				}
				updated.unzip match {
					case (chars, count) => (chars.mkString, count.sum)
				}
			}.unzip match {
				case (newLines, removedCounts) =>
					Grid(newLines) -> removedCounts.sum
			}
		}
	}
}
