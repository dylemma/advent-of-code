package io.dylemma.advent
package puzzles

import io.dylemma.advent.util.Matching._

import scala.util.chaining._

object Puzzle15 extends util.Puzzle {
	val Line = raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r
	case class Coord(x: Int, y: Int)
	case class BeaconPair(sensor: Coord, beacon: Coord) {
		lazy val dist = (beacon.x - sensor.x).abs + (beacon.y - sensor.y).abs
		def overlapWithLine(y: Int) = {
			val distToLine = (y - sensor.y).abs
			if (distToLine < dist) {
				val remainingDist = dist - distToLine
				Some((sensor.x - remainingDist, sensor.x + remainingDist))
			} else {
				None
			}
		}
	}
	val pairs = inputLines.map { case Line(AsInt(sx), AsInt(sy), AsInt(bx), AsInt(by)) => BeaconPair(Coord(sx, sy), Coord(bx, by)) }
	for (p <- pairs) println(s"$p - dist=${ p.dist }, overlaps with y=10 at ${ p.overlapWithLine(10) }")

	// given a list of (min, max) range pairs, sorted in increasing `min` order,
	// merge contiguous ranges to find the smallest list of "effective" ranges that cover the same coordinates
	def mergeRanges(ranges: List[(Int, Int)]): List[(Int, Int)] = ranges match {
		case (h @ (lMin, lMax)) :: (h2 @ (rMin, rMax)) :: tail =>
			if (rMin >= lMin && rMin <= (lMax + 1)) mergeRanges((lMin, rMax.max(lMax)) :: tail)
			else h :: mergeRanges(h2 :: tail)
		case list => list
	}

	def lineCoverage(y: Int) = mergeRanges(pairs.view.flatMap(_.overlapWithLine(y)).toList.sortBy(_._1))

	def part1(): String = {
		val targetLine = 2000000
		val beaconsOnLine = pairs.view.filter(_.beacon.y == targetLine).map(_.beacon.x).toSet
		lineCoverage(targetLine)
			.view
			.map { case (min, max) => min to max }
			.map { range => range.size - beaconsOnLine.count(range.contains) }
			.sum
			.toString
	}
	def part2(): String = {
		val searchSpace = 0 to 4000000

		searchSpace
			.view
			.flatMap { y =>
				// if (y % 100000 == 0) println(s"Searching... $y")
				lineCoverage(y) match {
					case (_, max) :: (min, _) :: _ => Some((max + 1) -> y)
					case _ => None
				}
			}
			.tapEach { gap => println(s"Found gap @ $gap")}
			.toList
			.pipe {
				case gap :: Nil => gap
				case Nil => throw new NoSuchElementException("No gap found!")
				case multi => throw new Exception(s"Ambiguous gaps: $multi")
			}
			.pipe { case (x, y) => (x.toLong * 4000000L) + y.toLong }
			.toString
	}
}
