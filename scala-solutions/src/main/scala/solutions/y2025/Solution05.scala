package io.dylemma.aoc
package solutions.y2025

import Utils.{ AsLong, AsTuple2 }

import scala.annotation.tailrec

object Solution05 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		val (rangeLines, itemLines) = input.linesIterator.span(_.nonEmpty)
		val ranges = rangeLines.map { line =>
			line.split('-') match {
				case AsTuple2(AsLong(l), AsLong(r)) => Span(l, r)
				case other => throw new IllegalArgumentException(s"Invalid range: '$line'")
			}
		}.toList
		val items = itemLines.drop(1).map(_.toLong).toList
		log.debug(s"Parsed ranges: $ranges, and items: $items")

		if (shouldRunPart1) {
			val freshItemCount = items.count { item =>
				ranges.exists(_ contains item)
			}
			log.info(s"Part 1: $freshItemCount items are fresh")
		}

		if (shouldRunPart2) {
			val coveredItemCount = countCoveredItems(ranges)
			log.info(s"Part 2: $coveredItemCount items are covered by the ranges")
		}
	}

	// basically NumericRange, but where `.length` won't throw exceptions
	// when the `max - min > Int.MaxValue`
	case class Span(min: Long, max: Long) {
		def contains(value: Long): Boolean = value >= min && value <= max
		def length = max - min + 1
	}

	def countCoveredItems(spans: List[Span]): Long = {
		val sortedSpans = spans.sortBy(_.min)

		// as long as we choose the next span in order of min value, we can avoid counting
		// duplicate items by tracking the current "search end" (i.e. max explored item)
		@tailrec
		def search(toVisit: List[Span], currentEnd: Long, accumCount: Long): Long = toVisit match {
			case Nil => accumCount
			case span :: rest =>
				if (span.max <= currentEnd) {
					// the entirety of this span was already covered; no new items counted
					log.debug(s"$span is fully covered by currentEnd=$currentEnd; skipping")
					search(rest, currentEnd, accumCount)
				} else if (span.min > currentEnd) {
					// skip over some invalid items and start from this new span
					log.debug(s"$span skips ahead and adds ${span.length} new items")
					search(rest, span.max, accumCount + span.length)
				} else {
					// span overlaps with previous coverage; only count the new items
					val newItemCount = span.max - currentEnd
					log.debug(s"$span adds $newItemCount after currentEnd=$currentEnd")
					search(rest, span.max, accumCount + newItemCount)
				}
		}

		search(sortedSpans, -1L, 0L)
	}
}
