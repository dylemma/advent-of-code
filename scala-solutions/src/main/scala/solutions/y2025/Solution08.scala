package io.dylemma.aoc
package solutions.y2025

import Utils.AsInt

import scala.annotation.tailrec

object Solution08 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		val positions = input.linesIterator.map { line =>
			line.split(',') match {
				case Array(AsInt(x), AsInt(y), AsInt(z)) => Pos(x, y, z)
				case _ => throw new IllegalArgumentException(s"Invalid position line: '$line'")
			}
		}.toList

		val pairsStartTime = System.currentTimeMillis
		val pairs = (for {
			aHeads <- positions.tails
			a <- aHeads.headOption.iterator
			b <- aHeads.tail.iterator
		} yield {
			val distSq = a.distSquaredTo(b)
			BoxPair(a, b, distSq)
		}).toVector.sortBy(_.distSquared)
		val pairsEndTime = System.currentTimeMillis
		
		log.debug(s"Computing and sorting pairs took ${pairsEndTime - pairsStartTime} ms")

		log.debug("First 10 box pairs by distance:")
		for (pair <- pairs.iterator.take(10)) log.debug(s"  $pair")

		log.debug(s"Got ${ pairs.size } pairs of boxes")
		
		val initialState = Circuits.initFrom(positions)

		if (shouldRunPart1) {

			val isExample = positions.size < 100
			val numMerges = if isExample then 10 else 1000

			val endState = pairs.take(numMerges).foldLeft(initialState) { (accum, pair) =>
				accum.merge(pair.a, pair.b)
			}
			val endSets = endState.sets.toList.sortBy(-_._2.size)
			for ((circuitId, set) <- endSets.iterator.take(3)) {
				if (set.size < 10) log.debug(s"Circuit $circuitId [size=${set.size}]: $set")
				else log.debug(s"Circuit $circuitId [size=${set.size}]")
			}

			val sizeProduct = endSets.iterator.take(3).map(_._2.size.toLong).product
			log.info(s"Part 1: $sizeProduct")
		}

		if (shouldRunPart2) {
			
			@tailrec
			def search(state: Circuits, toVisit: Iterator[BoxPair]): BoxPair = {
				toVisit.nextOption() match {
					case None => throw new IllegalStateException("Ran out of box pairs to visit")
					case Some(pair) =>
						val nextState = state.merge(pair.a, pair.b)
						if (nextState.isFullymerged) pair
						else search(nextState, toVisit)
				}
			}
			val lastPair = search(initialState, pairs.iterator)
			log.debug(s"Final pair to finish merge: $lastPair")
			val xProduct = lastPair.a.x.toLong * lastPair.b.x.toLong
			log.info(s"Part 2: $xProduct")
		}
	}

	opaque type CircuitId = Int

	case class Pos(x: Int, y: Int, z: Int):
		def distSquaredTo(there: Pos) = {
			val dx = (there.x - x).toLong
			val dy = (there.y - y).toLong
			val dz = (there.z - z).toLong
			(dx * dx) + (dy * dy) + (dz * dz)
		}

	case class BoxPair(a: Pos, b: Pos, distSquared: Long)

	case class Circuits(membership: Map[Pos, CircuitId], sets: Map[CircuitId, Set[Pos]]):
		def isFullymerged = sets.size == 1
		def merge(a: Pos, b: Pos): Circuits = {
			val circuitA = membership(a)
			val circuitB = membership(b)
			if (circuitA == circuitB) this
			else {
				val circuitAMembers = sets(circuitA)
				val circuitBMembers = sets(circuitB)
				val (toKeep, toDiscard) =
					if (circuitAMembers.size >= circuitBMembers.size)
						(circuitA, circuitB)
					else
						(circuitB, circuitA)

				// update the membership so that all positions from the discarded CircuitId
				// now point to the kept CircuitId instead
				val newMembership = membership ++ sets(toDiscard).view.map { pos => pos -> toKeep }

				// remove the discarded CircuitId from the `sets`, and update the kept CircuitId
				// to now hold the combined set of positions from both circuits
				val combinedSet = circuitAMembers ++ circuitBMembers
				val newSets = sets - toDiscard + (toKeep -> combinedSet)
				Circuits(newMembership, newSets)
			}
		}
	object Circuits:
		def initFrom(positions: IterableOnce[Pos]): Circuits = {
			val membership = positions.iterator.zipWithIndex.toMap
			val sets = membership.view.map { case (pos, circuitId) => circuitId -> Set(pos) }.toMap
			Circuits(membership, sets)
		}
}
