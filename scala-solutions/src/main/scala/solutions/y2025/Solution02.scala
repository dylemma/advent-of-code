package io.dylemma.aoc
package solutions.y2025

import Utils.{ AsLong, AsTuple2 }

import scala.collection.immutable.NumericRange

object Solution02 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {

		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		val ranges = input
			.trim
			.split(',')
			.view.map { rawRange =>
				rawRange.split('-').toSeq match {
					case AsTuple2(AsLong(l), AsLong(r)) => l to r
					case other => throw new IllegalArgumentException(s"Invalid range: '$rawRange'")
				}
			}

		if (shouldRunPart1) {
			val part1Sum = ranges
				.flatMap { range =>
					val invalids = findInvalidIds(range)
					if (invalids.isEmpty) {
						log.debug("  invalids: Nil")
					} else {
						log.debug(s"  invalids: $invalids")
					}
					invalids
				}
				.sum
			log.info(s"Part 1 sum: $part1Sum")
		}

		if (shouldRunPart2) {
			val part2Sum = ranges
				.view
				.tapEach { r => log.debug(s"Input range: ${ r.start }-${ r.end }") }
				.map { r => part2InvalidIds(r.start, r.end) }
				.tapEach { invalids =>
					if (invalids.isEmpty) log.debug("  invalids: Nil")
					else log.debug(s"  invalids: $invalids")
				}
				.flatten
				.iterator
				.distinct
				.sum
			log.info(s"Part 2 sum: $part2Sum")
		}
	}

	// given the puzzle description examples like `99-115` has one invalid ID, `99`:
	// I infer that I could avoid iterating the full range 99-115, and instead iterate
	// over the string-wise "half" of the range, e.g. `9-11` in this case.
	// Mapped back to the "rull" range, that would be [99, 1010, 1111].
	//
	// For other examples like `998-1012`, I can pick `9-10` as the half-range,
	// which maps back to [99, 1010], where only `1010` is contained in the real range.
	def stringHalf(num: Long): Long = {
		val s = String.valueOf(num)
		s.take((s.length / 2).max(1)).toLong
	}
	def stringDouble(num: Long): Long = {
		(String.valueOf(num) * 2).toLong
	}

	def findInvalidIds(range: NumericRange.Inclusive[Long]) = {
		val halfStart = stringHalf(range.start)
		val halfEnd = stringHalf(range.end).max(halfStart)
		val halfRange = halfStart to halfEnd
		log.debug(s"halfRange($range) -> $halfRange")
		halfRange.view.map(stringDouble).filter(range.contains).toList
	}

	// for part 2, we not only allow for "half" ranges, but also "third" and
	// "quarter" and so on. For a range bound with N digits, we need to consider
	// repeatable patterns of length 1 up to N/2 digits.
	// The examples show some edge cases:
	// With the range `95-115`, the puzzle points out that `111` is an "invalid ID",
	// i.e. the `1` repeated three times. But the range substring approach used in
	// part 1 would only consider `9-9`...
	//
	// With a simpler example like `2121212118-2121212124`, it's more apparent that
	// we could try `2-2`, `21-21`, `212-212`, `2121-2121`, and `21212-21212` as our
	// fractional ranges.
	def part2InvalidIds(start: Long, end: Long): Set[Long] = {
		val startS = String.valueOf(start)
		val endS = String.valueOf(end)
		if (startS.length == endS.length) {
			val len = startS.length
			val hits = for {
				numDigits <- 1 to (len / 2)
				if len % numDigits == 0
				factor = len / numDigits
				startBase = startS.take(numDigits).toLong
				endBase = endS.take(numDigits).toLong
				n <- startBase to endBase
				nRepeated = (String.valueOf(n) * factor).toLong
				if nRepeated >= start && nRepeated <= end
			} yield nRepeated
			hits.toSet

		} else if (endS.length == startS.length + 1) {
			// I'm sure there's some better way to generalize this,
			// but the actual puzzle input only has instances of
			// this, e.g. `99-115` and `1-14`.
			// My approach is to split it into two sub-ranges:
			// where each sub-range has the same digit-length
			// in its start and end. E.g. `99-115` becomes
			// `[99-99, 100-115]`, and `1-14` becomes
			// `[1-9, 10-14]`.
			val nextPowerOfTen = Math.pow(10, startS.length).toLong
			part2InvalidIds(start, nextPowerOfTen - 1) ++ part2InvalidIds(nextPowerOfTen, end)
		} else {
			throw new IllegalArgumentException(s"Don't know how to solve $start-$end")
		}
	}
}
