package io.dylemma.aoc
package solutions.y2023

import scala.collection.mutable

object Solution11 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {
		val universe = parse(input)
		solve(universe, 2)
		solve(universe, 10)
		solve(universe, 100)
		solve(universe, 1000000)
	}

	case class Point(x: Int, y: Int)

	// collect the position of each '#' as a "galaxy"
	def parse(input: String): Universe = {
		val galaxies = List.newBuilder[Point]
		var gCount = 0

		for {(line, y) <- input.linesIterator.zipWithIndex} {
			for {(char, x) <- line.iterator.zipWithIndex if char == '#'} {
				gCount += 1
				galaxies += Point(x, y)
			}
		}

		new Universe(galaxies.result())
	}

	class Universe(points: List[Point]) {
		private lazy val occupiedX = points.iterator.map(_.x).toSet
		private lazy val occupiedY = points.iterator.map(_.y).toSet
		private val numUnoccupiedBeforeX = Utils.memoize { (x: Int) =>
			(0 until x).count(xi => !occupiedX.contains(xi))
		}
		private val numUnoccupiedBeforeY = Utils.memoize { (y: Int) =>
			(0 until y).count(yi => !occupiedY.contains(yi))
		}

		// get the expanded position of a point, when unoccupied rows/columns
		// grow by the given `factor` (1 = no expansion, 2 = double size, etc.)
		def getExpanded(point: Point, factor: Int) = {
			Point(
				x = point.x + (numUnoccupiedBeforeX(point.x) * (factor - 1)),
				y = point.y + (numUnoccupiedBeforeY(point.y) * (factor - 1)),
			)
		}

		// iterator over distinct pairs of galaxies
		def galaxyPairs: Iterator[(Point, Point)] = for {
			l1 <- points.tails
			if l1.lengthCompare(2) >= 0
			g1 = l1.head
			g2 <- l1.tail.iterator
		} yield (g1, g2)
	}

	// get the sum of the manhattan distances between all galaxy pairs,
	// after expanding the universe by the given `factor`
	def solve(universe: Universe, factor: Int): Unit = {
		val pairDistances = for {
			(g1, g2) <- universe.galaxyPairs
		} yield {
			val expandedG1 = universe.getExpanded(g1, factor)
			val expandedG2 = universe.getExpanded(g2, factor)
			val absDx = (expandedG1.x - expandedG2.x).abs
			val absDy = (expandedG1.y - expandedG2.y).abs
			(absDx + absDy).toLong
		}

		val totalPairDistances = pairDistances.sum
		log.info(s"Total pair distances (with expansion factor $factor): $totalPairDistances")
	}
}
