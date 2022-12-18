package io.dylemma.advent
package year2022

import util.Puzzle
import util.Matching._

import scala.collection.immutable.Queue
import scala.util.chaining._

object Puzzle11 extends Puzzle {
	/*
	 * Input Parsing Helpers
	 */
	val MonkeyIdR = raw"Monkey (\d+):".r.intoMatcher(AsInt)
	val StartingItemsR = raw"Starting items: ([\d, ]*)".r.unanchored
	val TestR = raw"Test: divisible by (\d+)".r.unanchored.intoMatcher(AsInt)
	val IfTrueR = raw"If true: throw to monkey (\d+)".r.unanchored.intoMatcher(AsInt)
	val IfFalseR = raw"If false: throw to monkey (\d+)".r.unanchored.intoMatcher(AsInt)
	object Operation {
		val R = raw"Operation: new = old ([\*\+]) (\d+|old)".r.unanchored
		val Op = Matcher.confidently[(Worry, Worry) => Worry] {
			case "*" => (a, b) => Worry(a.value * b.value)
			case "+" => (a, b) => Worry(a.value + b.value)
		}
		val Num = Matcher.confidently[Worry => Worry] {
			case "old" => identity
			case AsLong(num) => _ => Worry(num)
		}
		def unapply(line: String): Option[Worry => Worry] = line match {
			case R(Op(op), Num(toNum)) => Some { (w: Worry) => op(w, toNum(w)) }
			case _ => None
		}
	}

	/*
	 * Wrapper type for "worry" to help with changing the underlying integer representation
	 */
	case class Worry(value: Long) extends AnyVal {
		def / (i: Int) = Worry(value / i)
		def * (i: Int) = Worry(value * i)
		def + (i: Int) = Worry(value + i)
		def % (i: Int) = Worry(value % i)
		def divisibleBy(i: Int) = value % i == 0
	}

	/*
	 * Parsed Input
	 */
	val monkeys = inputLines
		.view
		.grouped(7).map(_.toList)
		.map {
			case MonkeyIdR(id) :: StartingItemsR(csv) :: Operation(op) :: TestR(testNum) :: IfTrueR(trueId) :: IfFalseR(falseId) :: _ =>
				val items = csv.split(',').map(_.trim.pipe(AsLong).pipe(Worry)).to(Queue)
				Monkey(id, items, 0, op, testNum, trueId, falseId)
			case _ => ???
		}
		.toVector
		.pipe { monkeysVec =>
			// the special sauce for part two: modulo the `op` results by the
			// product of all of the monkeys' "divisor" values
			val divisorProduct = monkeysVec.view.map(_.testDivisor).product
			monkeysVec.map { m =>
				m.copy(op = { w => m.op(w) % divisorProduct })
			}
		}
		.pipe(Monkeys)

	// Represents when a monkey throws an item to another monkey
	case class Throw(to: Int, value: Worry)

	// Mmm, Monkey https://www.youtube.com/watch?v=oD9nAHuZsJU
	case class Monkey(
		id: Int,
		items: Queue[Worry],
		totalInspected: Int,
		op: Worry => Worry,
		testDivisor: Int, //Worry => Boolean,
		ifTrue: Int,
		ifFalse: Int,
	) {
		def receive(worry: Worry) = copy(items = items.enqueue(worry))
		def takeTurn(divideWorry: Boolean) = {
			val throws = Iterator
				.unfold(items) { _.dequeueOption }
				.map(op)
				.map { w => if (divideWorry) w / 3 else w }
				.map { w =>
					if (w divisibleBy testDivisor) Throw(ifTrue, w)
					else Throw(ifFalse, w)
				}
				.toVector
			val newState = copy(items = Queue.empty, totalInspected = totalInspected + throws.length)
			throws -> newState
		}
		def debug() = {
			println(s"Monkey $id inspected items $totalInspected times.")
		}
	}

	// game state
	case class Monkeys(monkeys: Vector[Monkey]) {
		def afterRound(divideWorry: Boolean) = Monkeys {
			monkeys.indices.foldLeft(monkeys) { (vec, i) =>
				val (throws, monkeyI) = vec(i).takeTurn(divideWorry)
				throws
					.foldLeft(vec) { (v, t) => v.updated(t.to, v(t.to).receive(t.value)) }
					.updated(i, monkeyI)
			}
		}
		def debug() = {
			for ((m, i) <- monkeys.iterator.zipWithIndex) {
				println(s"Monkey $i: ${m.items.mkString(", ")} - has inspected ${m.totalInspected}")
			}
			println(s"Business level: $monkeyBusiness")
		}
		def monkeyBusiness = {
			monkeys.map(_.totalInspected).sortBy(-_).iterator.take(2).map(_.toLong).product
		}
	}

	// run solutions
	def part1(): String = Iterator.iterate(monkeys)(_.afterRound(divideWorry = true)).to(LazyList).apply(20).monkeyBusiness.toString
	def part2(): String = Iterator.iterate(monkeys)(_.afterRound(divideWorry = false)).to(LazyList).apply(10000).monkeyBusiness.toString
}
