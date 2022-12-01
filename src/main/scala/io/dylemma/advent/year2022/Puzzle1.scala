package io.dylemma.advent.year2022

import io.dylemma.advent.util.Inputs
import io.dylemma.spac.interop.fs2._
import io.dylemma.spac.{ Parser, Splitter }

import cats.effect.SyncIO

object Puzzle1 {
	private val lineGrouper = Splitter[String]
		.consecutiveMatches(_.nonEmpty)
		.joinBy(Parser.toList)
		.map(_.map(_.toInt))

	case class Elf(rations: List[Int]) {
		lazy val totalCalories = rations.sum
		override def toString = s"Elf(totalCalories: $totalCalories, rations: $rations)"
	}

	def main(args: Array[String]): Unit = {
		val elves = Inputs
			.lineStream[SyncIO]("/input_2022_1.txt")
			.through(lineGrouper.toPipe)
			.map(Elf)
			.compile
			.toVector
			.unsafeRunSync()

		for(elf <- elves) println(elf)

		val maxElf = elves.maxBy(_.totalCalories)
		println(s"Max calories: ${maxElf.totalCalories}")

		val topElves = elves.sortBy(-_.totalCalories)
		val top3Total = topElves.iterator.take(3).map(_.totalCalories).sum
		println(s"Top 3 elves' total calories: $top3Total")
	}

}
