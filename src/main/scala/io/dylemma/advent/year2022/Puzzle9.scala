package io.dylemma.advent.year2022

import io.dylemma.advent.util.Puzzle
import scala.util.chaining._

object Puzzle9 extends Puzzle {
	// basic 2D "Point" type with helpers related to the puzzle
	case class XY(x: Int, y: Int) {
		def +(d: XY) = XY(x + d.x, y + d.y)
		def -(that: XY) = XY(x - that.x, y - that.y)
		def clampUnit = XY(x.min(1).max(-1), y.min(1).max(-1))
		def touching(that: XY) = (that.x - x).abs <= 1 && (that.y - y).abs <= 1
	}

	// logic for how `t` should move to make sure it stays touching `h` (returns the next `t` value)
	def follow(h: XY, t: XY) =
		if (t touching h) t
		else (h - t).clampUnit + t

	// interpret input as a series of XY steps
	val dirs = Map("U" -> XY(0, 1), "D" -> XY(0, -1), "R" -> XY(1, 0), "L" -> XY(-1, 0))
	val Line = "([UDRL]) (\\d+)".r
	val steps = inputLines.view.flatMap { case Line(dir, count) => Iterator.fill(count.toInt)(dirs(dir)) }

	// Rope of an arbitrary length: stores the set of points visited by its last knot, as well as the current position of each knot
	case class Rope(tailVisited: Set[XY], knots: Vector[XY]) {
		def step(d: XY) = {
			val nextTails = knots.tail.scanLeft(knots.head + d)(follow)
			Rope(tailVisited + nextTails.last, nextTails)
		}
		def debug(xRange: Range, yRange: Range, includeVisited: Boolean): String = {
			val sb = new StringBuilder
			yRange.foreach { y =>
				xRange.foreach { x =>
					val pos = XY(x, y)
					val char = knots.indexWhere(_ == pos) match {
						case -1 if includeVisited && tailVisited(pos) => '#'
						case -1 if x == 0 && y == 0 => 's'
						case -1 => '.'
						case 0 => 'H'
						case i => String.valueOf(i).charAt(0)
					}
					sb append char
				}
				sb.append('\n')
			}
			sb.result()
		}
		def debug(includeVisited: Boolean): String = {
			val points = tailVisited.view ++ knots.view
			val maxX = points.map(_.x).max
			val minX = points.map(_.x).min
			val maxY = points.map(_.y).max
			val minY = points.map(_.y).min
			debug(minX to maxX, maxY to minY by -1, includeVisited)
		}
	}
	object Rope {
		def apply(start: XY, length: Int): Rope = Rope(Set(start), Vector.fill(length)(start))
	}

	def part1(): String = {
		val finalState = steps.foldLeft(Rope(XY(0, 0), 2)) { _.step(_)/*.tap(r => println(r.debug(false)))*/ }
		println(finalState.debug(true))
		finalState.tailVisited.size.toString
	}
	def part2(): String = {
		val finalState = steps.foldLeft(Rope(XY(0, 0), 10)) { _.step(_)/*.tap(r => println(r.debug(false)))*/ }
		println(finalState.debug(true))
		finalState.tailVisited.size.toString
	}
}
