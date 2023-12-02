package io.dylemma.advent
package puzzles

import util.Matching._

import scala.annotation.tailrec

object Puzzle14 extends util.Puzzle {

	case class Coord(x: Int, y: Int)
	def line(a: Coord, b: Coord) = {
		require(a.x == b.x || a.y == b.y) // vertical XOR horizontal
		for {
			x <- (a.x.min(b.x) to a.x.max(b.x)).view
			y <- (a.y.min(b.y) to a.y.max(b.y)).view
		} yield Coord(x, y)
	}

	case class Domain(knownRange: Option[(Int, Int)]) {
		def + (value: Int) = knownRange match {
			case None => Domain(Some(value -> value))
			case Some((min, max)) => Domain(Some(min.min(value) -> max.max(value)))
		}
		def asRange = knownRange match {
			case None => 0 until 0
			case Some((min, max)) => min to max
		}
		def max = knownRange.fold(Int.MinValue)(_._2)
	}

	val sandXMovement = List(0, -1, 1)

	case class Grid(tiles: Map[Coord, Char], floor: Option[Int], domainX: Domain, domainY: Domain) {
		def this() = this(Map.empty, None, Domain(None), Domain(None))
		def updated(coord: Coord, value: Char) = {
			Grid(tiles.updated(coord, value), floor, domainX + coord.x, domainY + coord.y)
		}
		def withAutoFloor = withFloor(domainY.max + 2)
		def withFloor(floorY: Int) = copy(floor = Some(floorY))
		def toDebugString = {
			val sb = new StringBuilder
			for (y <- (domainY + 0 + floor.getOrElse(0)).asRange) {
				if (floor contains y) {
					sb.append("FLR ")
					for (_ <- domainX.asRange) sb.append('#')
				} else {
					sb.append(f"$y%3d ")
					for (x <- domainX.asRange) sb.append { tiles.getOrElse(Coord(x, y), '.') }
				}
				sb.append('\n')
			}
			sb.result()
		}
		def debugPrint() = println(toDebugString)
		def isBlocked(coord: Coord) = tiles.get(coord).exists { c => c == '#' || c == 'o' } || floor.fold(false)(coord.y >= _)
		@tailrec final def findSandLanding(from: Coord): Option[Coord] = {
			if (floor.isDefined || domainY.asRange.contains(from.y + 1)) {
				sandXMovement.view.map(dx => Coord(from.x + dx, from.y + 1)).find(!isBlocked(_)) match {
					case None => Some(from)
					case Some(next) => findSandLanding(next)
				}
			} else {
				// sand will fall forever
				None
			}
		}
	}

	val grid = inputLines
		.map { _.split(" -> ").view.map { coord => coord.split(',') match { case Array(AsInt(x), AsInt(y)) => Coord(x, y) } }.toVector }
		.flatMap { _.sliding(2).flatMap { case v => line(v(0), v(1)) } }
		.foldLeft(new Grid) { _.updated(_, '#') }
		.updated(Coord(500, 0), 'x')

	grid.debugPrint()

	val spawnPoint = Coord(500, 0)
	@tailrec def evolve(grid: Grid, sandCount: Int): (Grid, Int) = {
		grid.findSandLanding(spawnPoint) match {
			case None => grid -> sandCount
			case Some(`spawnPoint`) => grid.updated(spawnPoint, 'o') -> (sandCount + 1)
			case Some(rest) => evolve(grid.updated(rest, 'o'), sandCount + 1)
		}
	}

	def part1(): String = {
		val (finalGrid, sandCount) = evolve(grid, 0)
		finalGrid.debugPrint()
		sandCount.toString
	}
	def part2(): String = {
		val (finalGrid, sandCount) = evolve(grid.withAutoFloor, 0)
		finalGrid.debugPrint()
		sandCount.toString
	}
}
