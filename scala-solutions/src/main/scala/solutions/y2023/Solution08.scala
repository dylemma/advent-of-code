package io.dylemma.aoc
package solutions.y2023

import Utils.AsTuple2

import scala.annotation.tailrec

object Solution08 extends Solution with Logging {

	def run(input: String, args: List[String]): Unit = { 
		val doProof = args.contains("proof")
		val explicitlyPart1 = args.contains("part1") || args.contains("1")
		val explicitlyPart2 = args.contains("part2") || args.contains("2")
		
		val shouldRunPart1 = explicitlyPart1 || (!explicitlyPart1 && !explicitlyPart2)
		val shouldRunPart2 = explicitlyPart2 || (!explicitlyPart1 && !explicitlyPart2)
	
		val (directions, network) = parseInput(input.linesIterator)

		if (shouldRunPart1) new Part1(directions, network).run()
		if (shouldRunPart2) new Part2(directions, network).run(doProof)
	}

	enum Direction:
		case Left, Right

	case class Address(key: String)
	case class Node(left: Address, right: Address):
		def get(dir: Direction): Address = dir match
			case Direction.Left => left
			case Direction.Right => right

	private object parseInput {
		private val NetworkPattern = raw"(\w+) = \((\w+), (\w+)\)".r

		def apply(lines: Iterator[String]): (Vector[Direction], Map[Address, Node]) = {
			// parse first line as R/L directions
			val firstLine = lines.next().iterator.map {
				case 'L' => Direction.Left
				case 'R' => Direction.Right
				case c => throw new IllegalArgumentException(s"Unexpected letter on directions line: '$c'")
			}.toVector

			lines.next() // skip blank line

			// parse all the remaining lines as `Address = Node` mappings
			val network = lines.map {
				case NetworkPattern(addr, left, right) =>
					Address(addr) -> Node(Address(left), Address(right))
				case s => throw new IllegalArgumentException(s"Invalid network line: '$s'")
			}.toMap

			firstLine -> network
		}
	}

	private class Part1(directions: Vector[Direction], network: Map[Address, Node]) {
		private val start = Address("AAA")
		private val goal = Address("ZZZ")

		@tailrec
		final def search(current: Address, directionsItr: Iterator[Direction], numSteps: Int): Int = {
			if (current == goal) {
				numSteps // end recursion
			} else {
				val direction = directionsItr.next()
				val node = network.getOrElse(current, throw new IllegalArgumentException(s"Address not found in network: $current"))
				val nextAddr = node.get(direction)
				search(nextAddr, directionsItr, numSteps + 1)
			}
		}

		def run(): Unit = {
			val steps = search(start, Iterator.continually(directions).flatten, 0)
			log.info(s"Reached goal in $steps steps")
		}
	}

	private class Part2(directions: Vector[Direction], network: Map[Address, Node]) {

		extension (addr: Address)
			def isStart: Boolean = addr.key.endsWith("A")
			def isExit: Boolean = addr.key.endsWith("Z")

		opaque type DirectionIndex = Int

		extension (dirIndex: DirectionIndex)
			def next: DirectionIndex = (dirIndex + 1) % directions.length

		opaque type StepCount = Long

		extension (stepCount: StepCount)
			def toInt: Int = stepCount.toInt
			def inc: StepCount = stepCount + 1
			def toDirectionIndex: DirectionIndex = (stepCount % directions.length).toInt

		@tailrec
		private final def computeNextExit(current: Address, directionIndex: DirectionIndex, stepCount: StepCount): (Address, StepCount) = {
			if (current.isExit && stepCount > 0) {
				current -> stepCount
			} else {
				val direction = directions(directionIndex)
				val node = network.getOrElse(current, throw new IllegalArgumentException(s"Address not found in network: $current"))
				computeNextExit(node.get(direction), directionIndex.next, stepCount.inc)
			}
		}

		// (startAddress, directionIndex) -> (exitAddress, stepsTakenToExit)
		private val cache = collection.mutable.Map.empty[(Address, DirectionIndex), (Address, StepCount)]
		def getNextExit(current: Address, directionIndex: DirectionIndex): (Address, StepCount) = {
			cache.getOrElseUpdate(current -> directionIndex, {
				computeNextExit(current, directionIndex, 0L)
			})
		}

		case class GhostState(current: Address, numSteps: StepCount):
			def stateAtNextExit: GhostState = {
				val (a, n) = getNextExit(current, numSteps.toDirectionIndex)
				log.debug(s"Ghost walks +$n steps to reach $a")
				GhostState(a, numSteps + n)
			}

		case class SearchState(ghosts: List[GhostState]):
			lazy val maxNumSteps: StepCount = ghosts.iterator.map(_.numSteps).max
			def allAtExit: Boolean = ghosts.forall(_.current.isExit)
			def allAligned: Boolean = ghosts.view.map(_.numSteps).distinct.sizeCompare(1) == 0
			def step: SearchState = {
				SearchState(ghosts.map { state =>
					if (state.numSteps < maxNumSteps || state.numSteps == 0) {
						val result = state.stateAtNextExit
						log.debug(s"$state advances to $result...")
						result
					} else {
						log.debug(s"$state waits for the other ghosts...")
						state
					}
				})
			}

		def run(doProof: Boolean): Unit = {
			val ghosts = network.keysIterator.filter(_.isStart).map(GhostState(_, 0)).toList

			log.debug(s"Starting state: $ghosts")

			// Key observation:
			// The `SearchState`'s `step` function turns out to be cyclical.
			// From a given starting point to its corresponding exit, and
			// from that exit to the next exit (which turns out to be itself),
			// is always a number of steps that is a multiple of the directions'
			// length. Also by observation, that multiple is a prime number.
			// Therefore, each ghost will only align with others at the exits
			// after a number of such cycles that is the product of those primes,
			// times the directions length.

			if (doProof) {
				log.info("Simulating ghost movements to show cyclical behavior...")
				for (ghost <- ghosts) {
					val stepCountsPerExit = Iterator
						.iterate(ghost)(_.stateAtNextExit)
						.sliding(2)
						.map { case AsTuple2(before, after) =>
							val stepsTaken = after.numSteps.toInt - before.numSteps.toInt
							require(stepsTaken % directions.length == 0, "Steps taken to exit is not multiple of directions length")
							stepsTaken / directions.length
						}
						.take(10)
						.toSet
					require(stepCountsPerExit.size == 1, s"Ghost at $ghost has non-cyclical step counts per exit: $stepCountsPerExit")
					log.info(s"For 10 iterations, ghost starting at ${ghost.current} took ${stepCountsPerExit.head} * ${directions.length} steps to reach the next exit")
				}
			}

			val stepsToAlignment: Long = SearchState(ghosts).step.ghosts.map(_.numSteps).map { stepCount =>
				require(stepCount % directions.length == 0, "Step count to exit is not multiple of directions length")
				stepCount / directions.length
			}.product * directions.length

			log.info(s"Steps to alignment for all ghosts: $stepsToAlignment")
		}
	}
}
