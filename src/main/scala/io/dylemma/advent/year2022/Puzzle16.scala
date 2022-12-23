package io.dylemma.advent
package year2022

import io.dylemma.advent.util.Matching._

import scala.util.chaining._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

import cats.data.WriterT

object Puzzle16 extends util.Puzzle {
	case class ValveId(id: String) extends AnyVal {
		override def toString = id
	}
	case class Valve(id: ValveId, flowRate: Int, tunnelDestinations: List[ValveId])
	val ValveLine = raw"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)".r
	val valves = inputLines
		.view
		.map { case ValveLine(id, AsInt(rate), rawDests) => Valve(ValveId(id), rate, rawDests.split(", ").view.map(_.trim).map(ValveId).toList) }
		.map { v => v.id -> v }
		.toMap

	def bfs(from: ValveId, to: ValveId) = {
		@tailrec def search(q: Queue[(ValveId, Int)], visited: Set[ValveId]): Int = {
			val ((current, depth), tail) = q.dequeue
			if (visited(current)) search(tail, visited)
			else if (current == to) depth
			else search(tail.enqueueAll(valves(current).tunnelDestinations.view.map(_ -> (depth + 1))), visited + current)
		}

		search(Queue(from -> 0), Set.empty)
	}

	val usefulValves = valves.valuesIterator.filter(_.flowRate > 0).map(_.id).toSet
	val pathfindingValves = usefulValves + ValveId("AA")

	// from any given "pathfinding" valve, precompute the distance to each "useful" valve
	val distances = pathfindingValves.view.map { from =>
		from -> usefulValves.view.filterNot(_ == from).view.map { to => to -> bfs(from, to) }.toMap
	}.toMap

	sealed trait Step
	case class Open(id: ValveId, time: Int) extends Step
	case class Wait(time: Int) extends Step

	case class Path(steps: List[Step]) {
		def toDebugString = steps.mkString("[", " -> ", "]")
		def simulateRateIncreases: Iterator[Int] = Iterator.unfold(steps) {
			case Open(id, 1) :: tail => Some(valves(id).flowRate -> tail)
			case Open(id, t) :: tail => Some(0 -> (Open(id, t - 1) :: tail))
			case Wait(1) :: tail => Some(0 -> tail)
			case Wait(t) :: tail => Some(0 -> (Wait(t - 1) :: tail))
			case Nil => None
		}
	}

	def simulate(actorPaths: List[Path]) = {
		val rateIncreases = actorPaths.view.map(_.simulateRateIncreases).reduce(_.zip(_).map { case (a, b) => a + b })
		rateIncreases.foldLeft(0 -> 0) { case ((rate, accum), inc) =>
			(rate + inc, accum + rate)
		}._2
	}

	case class ActorState(
		currentMinute: Int, // start at 0
		pos: ValveId,
		closedValves: Set[ValveId],
		pathRev: List[Step],
	) {
		def path = Path(pathRev.reverse)
		// new state if the actor travels to the target valve then spends a minute opening it
		def open(target: ValveId) = {
			val travelTime =
				if (target == pos) 0
				else distances(pos)(target)
			ActorState(
				currentMinute + travelTime + 1,
				target,
				closedValves - target,
				Open(target, travelTime + 1) :: pathRev,
			)
		}
		// new state if the actor just sits around until the given minute
		def waitUntil(minute: Int) = {
			this.copy(
				currentMinute = minute,
				pathRev = Wait(minute - currentMinute) :: pathRev,
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
			pos = ValveId("AA"),
			closedValves = usefulValves,
			pathRev = Nil,
		)

		var count = 0
		val (bestPath, bestValue) = dfs(init, 30)
			.map { s => s -> simulate(s.path :: Nil) }
//			.tapEach { case (s, v) => println(s"search @ ${s.toDebugString}, value=$v")}
			.tapEach { _ => count += 1}
			.maxBy(_._2)
		println(s"Explored $count possible paths")
		println(s"Best path: ${bestPath.toDebugString}\nBest value: $bestValue")
		bestValue.toString
	}
	def part2(): String = {
		val init = ActorState(
			currentMinute = 0,
			pos = ValveId("AA"),
			closedValves = usefulValves,
			pathRev = Nil,
		)

		val pathPairs = for {
			myPath <- dfs(init, 26)
			eleInit = ActorState(
				currentMinute = 0,
				pos = ValveId("AA"),
				myPath.closedValves,
				pathRev = Nil,
			)
			elePath <- dfs(eleInit, 26)
		} yield myPath.path :: elePath.path :: Nil

		var count = 0
		val (bestPair, bestValue) = pathPairs
			.map { pair => pair -> simulate(pair) }
//			.tapEach { _ =>
//				count += 1
//				if (count % 1000000 == 1) println(s"searching... ${count - 1}")
//			}
			.maxBy(_._2)
		println(s"Explored $count possible paths")
		println("Best paths:")
		for (p <- bestPair) println(s"  ${p.toDebugString}")
		println(s"  value: $bestValue")

		// Best paths:
		//  [Open(IK,3) -> Open(VK,3) -> Open(IT,3) -> Open(WH,3) -> Open(WL,3) -> Open(HG,4) -> Wait(7)]
		//  [Open(EZ,3) -> Open(BV,3) -> Open(CF,3) -> Open(JX,4) -> Open(LB,3) -> Open(WM,4) -> Wait(6)]
		//  value: 2469
		bestValue.toString
	}
}
