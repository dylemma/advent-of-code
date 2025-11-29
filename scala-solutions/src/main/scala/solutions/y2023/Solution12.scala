package io.dylemma.aoc
package solutions.y2023

object Solution12 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {

		// part 1:
		val part1Counts = input
			.linesIterator
			.zipWithIndex
			.map { case (line, lineIndex) =>
				val inputLine = parseInputLine(line, foldingFactor = 1)
				val count = Part2.countConfigurations(inputLine)
				log.info(s"Line ${ lineIndex + 1 }: $line ->> $count configurations")
				if (log.isDebugEnabled) {
					val sizeFromIterator = Part1
						.iterateConfigurations(inputLine)
						.tapEach { config =>
							log.debug(s"  Valid configuration: " + config.map(b => if (b) '#' else '.').mkString)
						}
						.size
					require(sizeFromIterator == count, s"Count mismatch: iterator size = $sizeFromIterator, cached count = $count")
				}
				count
			}
			.sum

		// part 2:
		val part2Counts = input
			.linesIterator
			.zipWithIndex
			.map { case (line, lineIndex) =>
				val inputLine = parseInputLine(line, foldingFactor = 5)
				val count = Part2.countConfigurations(inputLine)
				log.info(s"Line ${ lineIndex + 1 } (5x folded): $line ->> $count configurations")
				count
			}
			.sum

		log.info(s"Part 1 sum: $part1Counts")
		log.info(s"Part 2 sum: $part2Counts")
	}

	case class InputLine(
		brokenStates: List[TriState],
		damagedGroupSizes: List[Int],
	) {
		def toRaw = {
			val left = brokenStates.map {
				case TriState.Broken => '#'
				case TriState.Working => '.'
				case TriState.Unknown => '?'
			}.mkString
			val right = damagedGroupSizes.mkString(",")
			s"$left $right"
		}
	}

	def parseInputLine(line: String, foldingFactor: Int): InputLine = {
		val Array(left, right) = line.split(" ", 2)
		val unfoldedLeft = List.fill(foldingFactor)(left).mkString("?")
		val brokenStates = unfoldedLeft.view.map {
			case '#' => TriState.Broken
			case '.' => TriState.Working
			case '?' => TriState.Unknown
			case c => throw new IllegalArgumentException(s"Invalid character in input: '$c'")
		}.toList
		val damagedGroupSizes = right.split(',').view.map(_.toInt).toList
		val unfoldedDGS = List.fill(foldingFactor)(damagedGroupSizes).flatten
		InputLine(brokenStates, unfoldedDGS)
	}

	def validate(brokenStates: List[Boolean], damagedGroupSizes: List[Int]): Boolean = brokenStates match {
		case false :: tail => validate(tail, damagedGroupSizes)
		case Nil => damagedGroupSizes.isEmpty
		case remaining =>
			val (brokenRun, tail) = remaining.span(_ == true)
			val numBroken = brokenRun.size
			damagedGroupSizes match {
				case `numBroken` :: gsTail => validate(tail, gsTail)
				case _ => false
			}
	}

	enum TriState:
		case Broken
		case Working
		case Unknown

	/** Part 1 produces an iterator of all valid configurations, built via recursion.
	  * 
	  * At each step, it checks if the next character in the input string (interpreted
	  * to a `triStates` list) will satisfy, continue, start, or break a "run" of
	  * consecutive broken states; when a run is completed, it continues the search
	  * if the length of that run satisfies the head of the `damagedGroupSizes` list.
	  * 
	  * An `accumReverse` list is passed through the recursion, so when the search
	  * reaches a success state (consumed the whole `triStates` list with no
	  * `damagedGroupSizes` left), it can emit the `accumReverse.reverse` as an output.
	  * 
	  * The double-reverse is because scala's `List` has `O(1)` prepend but `O(N)` append,
	  * so we build the list with `N` steps, with a single `O(N)` reverse at the end.
	  */
	object Part1 {
		def iterateConfigurations(inputLine: InputLine): Iterator[List[Boolean]] = {
			findConfigurations(inputLine.brokenStates, Nil, 0, inputLine.damagedGroupSizes)
		}

		private def findConfigurations(triStates: List[TriState], accumReverse: List[Boolean], runLength: Int, damagedGroupSizes: List[Int]): Iterator[List[Boolean]] = triStates match {
			case TriState.Working :: tail =>
				if (runLength > 0) {
					// Previous steps were 'broken', and now that we got to a "working" state,
					// the run is over, so we compare it to the expected `damagedGroupSizes.head`
					// to see if it hat the correct length, only continuing if it was correct.
					damagedGroupSizes match {
						case `runLength` :: dgTail =>
							// recurse, appending `false` (not broken) to the `accum` list (prepend to accumReverse)
							findConfigurations(tail, false :: accumReverse, 0, dgTail)
						case other =>
							Iterator.empty
					}
				} else {
					// Skip past the `.` characters...
					findConfigurations(tail, false :: accumReverse, 0, damagedGroupSizes)
				}

			case TriState.Broken :: tail =>
				// we are either starting or continuing a run of broken ('#') states
				findConfigurations(tail, true :: accumReverse, runLength + 1, damagedGroupSizes)

			case Nil =>
				// we got to the end of the `triStates` input, and may or may not be in a "run"
				if (runLength > 0) {
					damagedGroupSizes match {
						case `runLength` :: Nil =>
							// This run matched the last `damagedGroupSize`, so we found a working pattern.
							// Emit the `accum` by reversing the `accumReverse`
							Iterator(accumReverse.reverse)
						case _ =>
							// either we ended with a wrong-length run, or there weren't any more expected
							// runs, so this was a failed fork
							Iterator.empty
					}
				} else {
					// we ended not in a run, so we should have no more expected damaged groups
					if (damagedGroupSizes.isEmpty) {
						// Emit the `accum` by reversing the `accumReverse`
						Iterator(accumReverse.reverse)
					} else {
						// there were still expected damaged groups, so this was a failed fork
						Iterator.empty
					}
				}

			case TriState.Unknown :: tail =>
				// for an unknown (`?`) step, try replacing it with `.` or `#`
				val resultsWhenWorking = findConfigurations(TriState.Working :: tail, accumReverse, runLength, damagedGroupSizes)
				val resultsWhenBroken = findConfigurations(TriState.Broken :: tail, accumReverse, runLength, damagedGroupSizes)
				resultsWhenBroken ++ resultsWhenWorking
		}
	}

	/** Part 2's solution uses the same general algorithm as in Part 1, but instead of returning
	  * an iterator, it only counts success states. Where an `Iterator(result)` would be emitted
	  * in Part 1, here we return `1L`, and where `Iterator.empty` would be returned, here we return `0L`.
	  * For the `?` step, we sum the counts from both branches.
	  * 
	  * Since the input size is multiplied for Part 2, and the search space is `O(2^N)`, an un-cached
	  * approach would end up re-evaluating states towards the end of the search many times over.
	  * Therefore, a cache is used to memoize the results of sub-calls to `countConfigurations`.
	  */
	object Part2 {
		def countConfigurations(inputLine: InputLine): Long = {
			countConfigurations(inputLine.brokenStates, 0, inputLine.damagedGroupSizes)
		}

		private val _configCountCache = collection.mutable.Map.empty[(List[TriState], Int, List[Int]), Long]

		private def countConfigurations(triStates: List[TriState], runLength: Int, damagedGroupSizes: List[Int]): Long = {
			val key = (triStates, runLength, damagedGroupSizes)
			_configCountCache.getOrElseUpdate((triStates, runLength, damagedGroupSizes),
				countConfigurationsUncached(triStates, runLength, damagedGroupSizes)
			)
		}

		private def countConfigurationsUncached(triStates: List[TriState], runLength: Int, damagedGroupSizes: List[Int]): Long = triStates match {
			case TriState.Working :: tail =>
				if (runLength > 0) {
					// Previous steps were 'broken', and now that we got to a "working" state,
					// the run is over, so we compare it to the expected `damagedGroupSizes.head`
					// to see if it hat the correct length, only continuing if it was correct.
					damagedGroupSizes match {
						case `runLength` :: dgTail =>
							// recurse, appending `false` (not broken) to the `accum` list (prepend to accumReverse)
							countConfigurations(tail, 0, dgTail)
						case other =>
							0L
					}
				} else {
					// Skip past the `.` characters...
					countConfigurations(tail, 0, damagedGroupSizes)
				}

			case TriState.Broken :: tail =>
				// we are either starting or continuing a run of broken ('#') states
				countConfigurations(tail, runLength + 1, damagedGroupSizes)

			case Nil =>
				// we got to the end of the `triStates` input, and may or may not be in a "run"
				if (runLength > 0) {
					damagedGroupSizes match {
						case `runLength` :: Nil =>
							// This run matched the last `damagedGroupSize`, so we found a working pattern.
							// Emit the `accum` by reversing the `accumReverse`
							1L
						case _ =>
							// either we ended with a wrong-length run, or there weren't any more expected
							// runs, so this was a failed fork
							0L
					}
				} else {
					// we ended not in a run, so we should have no more expected damaged groups
					if (damagedGroupSizes.isEmpty) {
						// Emit the `accum` by reversing the `accumReverse`
						1L
					} else {
						// there were still expected damaged groups, so this was a failed fork
						0L
					}
				}

			case TriState.Unknown :: tail =>
				// for an unknown (`?`) step, try replacing it with `.` or `#`
				val resultsWhenWorking = countConfigurations(TriState.Working :: tail, runLength, damagedGroupSizes)
				val resultsWhenBroken = countConfigurations(TriState.Broken :: tail, runLength, damagedGroupSizes)
				resultsWhenBroken + resultsWhenWorking
		}
	}
}
