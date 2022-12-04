package io.dylemma.advent.year2022

import io.dylemma.advent.util.Puzzle

object Puzzle4 extends Puzzle {
	val RangeExpr = raw"(\d+)-(\d+),(\d+)-(\d+)".r
	def ranges(line: String) = line match {
		case RangeExpr(a,b,c,d) => (a.toInt to b.toInt, c.toInt to d.toInt)
	}
	def rangeContains(x: Range.Inclusive, y: Range.Inclusive) = {
		(x.min >= y.min && x.max <= y.max) ||
			(y.min >= x.min && y.max <= x.max)
	}

	def part1(): String = {
		inputLines
			.iterator
			.map(ranges)
			.count { case (x, y) => rangeContains(x, y) }
			.toString
	}
	def part2(): String = {
		inputLines
			.map(ranges)
			.count { case (x, y) => x.toSet.intersect(y.toSet).nonEmpty}
			.toString
	}
}
