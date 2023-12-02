package io.dylemma.advent.util

abstract class Puzzle {
	lazy val inputLines = {
		val pkg = getClass.getPackage.getName
		val name = getClass.getSimpleName
		val PackagePattern = raw".*\.year(\d+)".r
		val NamePattern = raw"Puzzle(\d+)\$$?".r
		val year = pkg match {
			case PackagePattern(digits) => digits.toInt
			case _ => throw new IllegalArgumentException(s"non-conventional package name: '$pkg' - expected '[...].year20XX")
		}
		val puzzleNum = name match {
			case NamePattern(digits) => digits.toInt
			case _ => throw new IllegalArgumentException(s"non-conventional class name: '$name' - expected 'PuzzleN'")
		}
		val inputPath = s"/input_${year}_$puzzleNum.txt"
		Inputs.lines[Vector](inputPath)
	}

	def part1(): String
	def part2(): String

	def main(args: Array[String]): Unit = {
		val result1 = part1()
		println(s"Part 1 Result: $result1")

		val result2 = part2()
		println(s"Part 2 Result: $result2")
	}
}
