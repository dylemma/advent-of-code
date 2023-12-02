package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Puzzle

object Puzzle5 extends Puzzle {
	case class Blocks(cols: Vector[List[Char]]) {
		def moveBlock(fromIndex: Int, toIndex: Int, times: Int) = {
			val outTo = cols(fromIndex).iterator.take(times).foldLeft(cols(toIndex))(_ prepended _)
			val outFrom = cols(fromIndex).drop(times)
			Blocks(cols.updated(fromIndex, outFrom).updated(toIndex, outTo))
		}
		def moveBlockV2(fromIndex: Int, toIndex: Int, times: Int) = {
			val (moved, outFrom) = cols(fromIndex).splitAt(times)
			val outTo = cols(toIndex).prependedAll(moved)
			Blocks(cols.updated(fromIndex, outFrom).updated(toIndex, outTo))
		}
		def tops = cols.view.map(_.head)
	}

	private val blocks = {
		def asBlock(s: String) =
			if (s.startsWith("[") && s.length >= 2) Some(s.charAt(1))
			else None

		// first row is top, last is bottom
		val rows = inputLines
			.view
			.takeWhile(!_.startsWith(" "))
			.map { _.grouped(4).map(asBlock).toVector }
			.toVector
		val numCols = rows.view.map(_.length).max
		val cols = (0 until numCols).view.map { x =>
			rows.iterator.flatMap(row => row.applyOrElse(x, (_: Any) => None)).toList
		}.toVector
		Blocks(cols)
	}

	private val instructions = {
		val linesIter = inputLines.iterator
		var line = "..."
		while (line.nonEmpty && linesIter.hasNext) {
			line = linesIter.next()
		}
		val Insn = raw"move (\d+) from (\d+) to (\d+)".r
		linesIter.map {
			case Insn(times, from, to) => (from.toInt - 1, to.toInt - 1, times.toInt)
			case other => throw new IllegalArgumentException(s"bad instruction: '${other}'")
		}.toVector
	}



	def part1(): String = {
		instructions
			.foldLeft(blocks) { (blocks, insn) => (blocks.moveBlock _).tupled(insn) }
			.tops
			.mkString
	}
	def part2(): String = {
		instructions
			.foldLeft(blocks) { (blocks, insn) => (blocks.moveBlockV2 _).tupled(insn) }
			.tops
			.mkString
	}
}
