package io.dylemma.advent
package puzzles

import io.dylemma.advent.util.Matching._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Puzzle16 extends util.Puzzle {
	case class ValveId(index: Int) extends AnyVal {
		override def toString = valves(index).label
	}
	case class Valve(id: ValveId, label: String, flowRate: Int, tunnelDestinations: List[ValveId])

	val valves = {
		val ValveLine = raw"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)".r
		val parsed = inputLines
			.view
			.zipWithIndex
			.map { case (ValveLine(label, AsInt(rate), rawDests), idx) => (idx, label, rate, rawDests) }
			.toVector
		val idForLabel = parsed.view.map { case (idx, label, _, _) => label -> ValveId(idx) }.toMap
		parsed.map { case (idx, label, rate, rawDests) =>
			val dests = rawDests.split(", ").view.map(_.trim).map(idForLabel).toList
			Valve(ValveId(idx), label, rate, dests)
		}
	}

	val valveAA = valves.find(_.label == "AA").get.id

	def bfs(from: ValveId, to: ValveId) = {
		@tailrec def search(q: Queue[(ValveId, Int)], visited: Set[ValveId]): Int = {
			val ((current, depth), tail) = q.dequeue
			if (visited(current)) search(tail, visited)
			else if (current == to) depth
			else search(tail.enqueueAll(valves(current.index).tunnelDestinations.view.map(_ -> (depth + 1))), visited + current)
		}

		search(Queue(from -> 0), Set.empty)
	}

	val usefulValves = valves.view.filter(_.flowRate > 0).map(_.id).toSet
	val pathfindingValves = usefulValves + valveAA

	// from any given "pathfinding" valve, precompute the distance to each "useful" valve
	val distances = pathfindingValves.view.map { from =>
		from -> usefulValves.view.filterNot(_ == from).view.map { to => to -> bfs(from, to) }.toMap
	}.toMap

	sealed trait Step
	case class Open(id: ValveId, time: Int) extends Step
	case class Wait(time: Int) extends Step

	case class ActorState(
		currentMinute: Int, // start at 0
		pos: ValveId,
		closedValves: Set[ValveId],
		rate: Int,
		accum: Int,
		pathRev: List[Step],
	) {
		// new state if the actor travels to the target valve then spends a minute opening it
		def open(target: ValveId) = {
			val timeSpent = // travel time plus a minute to open the valve
				if (target == pos) 1
				else distances(pos)(target) + 1
			ActorState(
				currentMinute + timeSpent,
				target,
				closedValves - target,
				rate + valves(target.index).flowRate,
				accum + (rate * timeSpent),
				Open(target, timeSpent) :: pathRev,
			)
		}
		// new state if the actor just sits around until the given minute
		def waitUntil(minute: Int) = {
			val waitTime = minute - currentMinute
			ActorState(
				currentMinute = minute,
				pos,
				closedValves,
				rate,
				accum + (rate * waitTime),
				pathRev = Wait(waitTime) :: pathRev,
			)
		}

		def nextPossibleStates(timeLimit: Int): List[ActorState] = {
			if (currentMinute >= timeLimit) Nil
			else if (closedValves.isEmpty) waitUntil(timeLimit) :: Nil
			else waitUntil(timeLimit) :: closedValves.view.map(open).filterNot(_.currentMinute >= timeLimit).toList
		}

		def toDebugString = s"[${ pathRev.reverse.mkString(" -> ") }], time=${ currentMinute }"
	}

	def dfs(current: ActorState, timeLimit: Int): Iterator[ActorState] = current.nextPossibleStates(timeLimit) match {
		case Nil =>
			// println(s"Possible: ${current.toDebugString}")
			Iterator.single(current)
		case nexts => nexts.iterator.flatMap { s =>
			require(s.currentMinute > current.currentMinute)
			dfs(s, timeLimit)
		}
	}

	valves.foreach(println)
	for ((from, dists) <- distances) println(s"From $from, dists=$dists")

	def part1(): String = {

		val init = ActorState(
			currentMinute = 0,
			pos = valveAA,
			closedValves = usefulValves,
			rate = 0,
			accum = 0,
			pathRev = Nil,
		)

		var count = 0
		val bestPath = dfs(init, 30)
			.tapEach { _ => count += 1 }
			.maxBy(_.accum)
		println(s"Explored $count possible paths")
		println(s"Best path: ${ bestPath.toDebugString }")
		bestPath.accum.toString
	}
	def part2(): String = {
		val init = ActorState(
			currentMinute = 0,
			pos = valveAA,
			closedValves = usefulValves,
			rate = 0,
			accum = 0,
			pathRev = Nil,
		)

		val statePairs = for {
			myPath <- dfs(init, 26)
			eleInit = ActorState(
				currentMinute = 0,
				pos = valveAA,
				myPath.closedValves,
				rate = 0,
				accum = 0,
				pathRev = Nil,
			)
			elePath <- dfs(eleInit, 26)
		} yield myPath :: elePath :: Nil

		var count = 0
		val (bestPair, bestValue) = statePairs
			.map { pair => pair -> pair.view.map(_.accum).sum }
			.tapEach { _ =>
				count += 1
				if (count % 1000000 == 1) println(s"searching... ${ count - 1 }")
			}
			.maxBy(_._2)
		println(s"Explored $count possible paths")
		println("Best paths:")
		for (p <- bestPair) println(s"  ${ p.toDebugString }")
		println(s"  value: $bestValue")

		// Best paths:
		//  [Open(IK,3) -> Open(VK,3) -> Open(IT,3) -> Open(WH,3) -> Open(WL,3) -> Open(HG,4) -> Wait(7)]
		//  [Open(EZ,3) -> Open(BV,3) -> Open(CF,3) -> Open(JX,4) -> Open(LB,3) -> Open(WM,4) -> Wait(6)]
		//  value: 2469
		// [success] Total time: 578 s (09:38),
		bestValue.toString
	}
}
