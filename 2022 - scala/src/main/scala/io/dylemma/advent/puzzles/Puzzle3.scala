package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Puzzle

object Puzzle3 extends Puzzle {

	class Rucksack(line: String) {
		val (sack1, sack2) = line.splitAt(line.length / 2)
		val letters1 = sack1.toSet
		val letters2 = sack2.toSet
		val overlap = letters1.intersect(letters2)
	}

	def priority(letter: Char) = {
		if (letter > 'Z') {
			letter - 'a' + 1
		} else {
			letter - 'A' + 27
		}
	}

	def part1(): String = {
		inputLines
			.view
			.flatMap(new Rucksack(_).overlap)
			.map(priority)
			.sum
			.toString
	}

	def part2(): String = {
		inputLines
			.grouped(3)
			.flatMap { v =>
				v(0).toSet intersect v(1).toSet intersect v(2).toSet
			}
			// .tapEach(println)
			.map(priority)
			.sum
			.toString
	}
}
