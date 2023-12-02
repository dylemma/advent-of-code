package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Puzzle

import scala.collection.View
import scala.util.chaining._

object Puzzle10 extends Puzzle {

	sealed trait Op {
		def :*:(x: Int): Int
	}
	object Op {
		case object Noop extends Op {
			def :*:(x: Int) = x
		}
		case class Add(dx: Int) extends Op {
			def :*:(x: Int) = x + dx
		}
	}

	val opsByCycle: View[Op] = inputLines.view.map(_.split(' ')).flatMap {
		case Array("noop") => Iterator.single(Op.Noop)
		case Array("addx", dx) => Iterator(Op.Noop, Op.Add(dx.toInt))
	}

	def part1(): String = {
		opsByCycle
			.scanLeft(1) { _ :*: _ } // run commands
			.zipWithIndex.map { case (x, i) => (x, i + 1) } // count cycles, adjusting for 0-indexing
			.filter { case (_, i) => (i - 20) % 40 == 0 } // pick the cycles we care to investigate
			.tapEach { case (x, i) => println(s"Cycle $i, x=$x") } // debug
			.map { case (x, i) => x * i } // compute signal strength
			.tapEach { str => println(s"  signal strength = $str") } // debug
			.sum
			.toString
	}
	def part2(): String = {
		opsByCycle
			.scanLeft(1) { _ :*: _ } // run commands
			.zipWithIndex // count cycles, adjusting for 0-indexing
			.map { case (x, i) => if ((x - (i % 40)).abs <= 1) '#' else '.' } // print
			.grouped(40) // arrange into lines
			.map(line => (line ++ "\n").mkString)
			.mkString
			.pipe("\n" + _)
	}
}
