package io.dylemma.aoc
package solutions.y2023

import Utils.*

import scala.annotation.tailrec
import scala.util.chaining.*

object Solution09 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {

		val inputHistories: List[History] = input
			.linesIterator
			.map { line =>
				line.split(' ').view.map {
					case AsLong(n) => n
					case other => throw new IllegalArgumentException(s"Invalid number: '$other'")
				}.toVector
			}
			.map(History.apply)
			.toList

		log.debug(s"Parsed inputs: $inputHistories")

		new Part1().run(inputHistories)
	}

	case class History(numbers: Vector[Long]):
		override def toString = s"History(${ numbers.mkString(", ") })"

		def allZero = numbers.forall(_ == 0L)

		def appendWithDiff(diff: Long) = History(numbers :+ (numbers.last + diff))

		def derivative: History = numbers
			.slidingPairs
			.map { (a, b) => b - a }
			.toVector
			.pipe(History.apply)

	enum HistoryChain {
		case Top(history: History)
		case Derived(derivative: History, parent: HistoryChain)

		// extends the History, keeping the whole chain intact
		def extendAsChain(bottomDiff: Long): HistoryChain = this match {
			case Top(history) => Top(history.appendWithDiff(bottomDiff))
			case Derived(derivative, parent) =>
				val extendedDerivative = derivative.appendWithDiff(bottomDiff)
				val diffForParent = extendedDerivative.numbers.last
				Derived(extendedDerivative, parent.extendAsChain(diffForParent))
		}

		// extends the Top history, discarding the derivative chain
		def extendAsHistory(bottomDiff: Long): History = this match {
			case Top(history) => history.appendWithDiff(bottomDiff)
			case Derived(derivative, parent) =>
				val diffForParent = derivative.appendWithDiff(bottomDiff).numbers.last
				parent.extendAsHistory(diffForParent)
		}

		def top: History = this match {
			case Top(history) => history
			case Derived(_, parent) => parent.top
		}
	}

	class Part1 {
		def run(histories: List[History]): Unit = {
			val sum = (for (history <- histories.iterator) yield {
				log.debug(s"History: $history")

				val extended = buildChain(history).extendAsHistory(0)
				log.debug(s"Extended: $extended")

				extended.numbers.last
			}).sum
			log.info(s"Part 1: sum of extended histories is $sum")
		}

		def buildChain(history: History): HistoryChain = {
			@tailrec
			def loop(parent: HistoryChain, current: History): HistoryChain = {
				val derivative = current.derivative
				if (derivative.allZero) {
					HistoryChain.Derived(derivative, parent)
				} else {
					loop(HistoryChain.Derived(derivative, parent), derivative)
				}
			}

			loop(HistoryChain.Top(history), history)
		}
	}
}
