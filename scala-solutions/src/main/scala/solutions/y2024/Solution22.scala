package io.dylemma.aoc
package solutions.y2024

import Utils.*

import scala.util.chaining.*

import cats.syntax.foldable.*

object Solution22 extends Solution.NoArgs with Logging {

	def run(input: String): Unit = {

		// Part 1: get the sum of each buyer's 2000th secret number
		val p1Sum: Long = input.linesIterator.collect { case AsLong(num) =>
			log.debug(s"line: $num")
			val evolved = evolve2000(num)
			log.debug(f"0x$evolved%06x = $evolved%d")
			evolved
		}.sum
		log.info(s"Part 1 sum: $p1Sum")

		// Part 2: find the change pattern with the highest total price across all buyers
		val (bestChanges: Changes, bestPrice: Long) = input
			.linesIterator // each line of the input
			.flatMap { AsLong.unapply } // ...is the 'seed' for a buyer
			.map(indexBuyer) // find the price for each buyer's changes
			.to(LazyList) // satisfy Cats's `foldable` requirement (there is no Foldable[Iterator])
			.combineAll // merge each index, summing the total price for each change
			.tap { combinedIndex =>
				log.info(s"Computed total price of ${ combinedIndex.size } change patterns")
			}
			.maxBy(_._2) // find the entry in the combined map with the highest total price
		log.info(s"Best change pattern: ${ bestChanges.formatted } with total price $bestPrice")

	}

	// sanity check my hex and bit-shift reasoning
	require(16777216 == 0x1000000, "Expected 0x1000000 to be 16777216")
	require(1 << 24 == 0x1000000, "Expected 1 << 24 to be 16777216")
	require(1 << 11 == 2048, "Expected 1 << 11 to be 2048")
	require(1 << 6 == 64, "Expected 1 << 6 to be 64")

	// PRNG function for a buyer's secret evolution,
	// as described in the puzzle
	def evolve(secret: Long): Long = {
		// multiply by 64, mix and prune
		val step1 = ((secret << 6) ^ secret) % 0x1000000

		// divide by 32, mix and prune
		val step2 = ((step1 >> 5) ^ step1) % 0xFFFFFF

		// multiply by 2048, mix and prune
		val step3 = ((step2 << 11) ^ step2) % 0x1000000

		step3
	}

	// get the 2000th evolution of a buyer's secret, given the 1st
	def evolve2000(secret: Long): Long = {
		(1 to 2000).foldLeft(secret)((n, _) => evolve(n))
	}

	// view of a buyer's secret along with the price diff since the previous secret
	case class BuyerState(secret: Long, diffSincePrev: Int):
		lazy val price = secret % 10
		def formatted: String = f"$secret%10d: $price ($diffSincePrev)"
		def next = {
			val nextSecret = evolve(secret)
			val nextPrice = nextSecret % 10
			val nextDiff = (nextPrice - price).toInt
			BuyerState(nextSecret, nextDiff)
		}

	object BuyerState:
		def iterateFrom(seed: Long): Iterator[BuyerState] =
			Iterator.iterate(BuyerState(seed, 0))(_.next).drop(1)

	// view of the 4 most recent changes in price based on some buyer's secret evolution
	case class Changes(diffs: (Int, Int, Int, Int)):
		def formatted = s"[${ diffs._1 }, ${ diffs._2 }, ${ diffs._3 }, ${ diffs._4 }]"
	object Changes:
		def fromList(diffs: Seq[Int]): Changes = diffs match
			case Seq(a, b, c, d) => Changes((a, b, c, d))
			case _ => throw new IllegalArgumentException(s"Expected list of 4 diffs, got: $diffs")

	// view of a buyer's current price along with the recent changes that led to it
	case class TriggerState(currentPrice: Long, changes: Changes)

	// helper to compute the TriggerStates from a stream of BuyerStates
	extension (it: Iterator[BuyerState])
		def toTriggers: Iterator[TriggerState] = it.sliding(4).map { states =>
			val price = states.last.price
			val changes = Changes fromList states.map(_.diffSincePrev)
			TriggerState(price, changes)
		}

	// build an index of prices by change patterns for a given buyer,
	// including only the first occurrence of each change pattern
	def indexBuyer(seed: Long): Map[Changes, Long] = BuyerState
		.iterateFrom(seed)
		.take(2000)
		.toTriggers
		.foldLeft(Map.empty[Changes, Long]) { case (priceMap, state) =>
			// insert the price of the current state in the map,
			// but only if it wasn't already in the map
			priceMap.updatedWith(state.changes) {
				case None => Some(state.currentPrice)
				case Some(existingPrice) => Some(existingPrice)
			}
		}
}
