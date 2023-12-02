package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Puzzle

object Puzzle6 extends Puzzle {
	def findStartMarker(size: Int, chars: Iterator[Char]) = chars
		.sliding(size)
		.indexWhere { _.toSet.sizeCompare(size) == 0 }
		.+(size) // my commitment to chained method call style is unwavering

	def part1(): String = {
		findStartMarker(4, inputLines.head.iterator).toString


	}
	def part2(): String = {
		findStartMarker(14, inputLines.head.iterator).toString
	}
}
