package io.dylemma.advent
package year2022

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._

object Puzzle12 extends util.Puzzle {

	case class Address(x: Int, y: Int) {
		def distTo(there: Address) = (there.x - x).abs + (there.y - y).abs
		def cardinalNeighbors = List(
			Address(x, y + 1),
			Address(x + 1, y),
			Address(x, y - 1),
			Address(x - 1, y),
		)
	}

	case class Elevation(value: Int, isStart: Boolean, isEnd: Boolean) {
		def isReachableFrom(here: Elevation) = value - here.value <= 1
		def toChar: Char = {
			if (isStart) 'S'
			else if (isEnd) 'E'
			else ('a' + value).toChar
		}
	}
	object Elevation {
		def fromChar(c: Char): Elevation = c match {
			case 'S' => Elevation(0, isStart = true, isEnd = false)
			case 'E' => fromChar('z').copy(isEnd = true)
			case alpha => Elevation(alpha - 'a', isStart = false, isEnd = false)
		}
	}

	case class TopoMap(grid: Vector[Vector[Elevation]]) {
		lazy val startAddress = {
			(for {
				(row, y) <- grid.view.zipWithIndex
				(e, x) <- row.view.zipWithIndex
				if e.isStart
			} yield Address(x, y)).head
		}
		lazy val goalAddress = {
			(for {
				(row, y) <- grid.view.zipWithIndex
				(e, x) <- row.view.zipWithIndex
				if e.isEnd
			} yield Address(x, y)).head
		}
		def get(address: Address) = grid.lift(address.y).flatMap(_.lift(address.x))
		def apply(address: Address) = grid(address.y)(address.x)
		def neighborsOf(address: Address) = {
			val here = apply(address)
			address.cardinalNeighbors.view.filter { n =>
				get(n).exists(_ isReachableFrom here)
			}
		}
		def inverseNeighborsOf(address: Address) = {
			val here = apply(address)
			address.cardinalNeighbors.view.filter { n =>
				get(n).exists(here isReachableFrom _)
			}
		}
		def debugPrint() = {
			for (row <- grid) {
				for (e <- row) print(e.toChar)
				println()
			}
		}
	}

	val topoMap = inputLines.map { _.view.map(Elevation.fromChar).toVector }.reverse.pipe(TopoMap)
	topoMap.debugPrint()
	println(s"Try to get from ${ topoMap.startAddress } to ${ topoMap.goalAddress }")

	// Part 1: use A* search to get from the 'S' to the 'E'
	def aStar(
		topo: TopoMap,
		d: (Address, Address) => Int,
	) = {
		val start = topo.startAddress
		val goal = topo.goalAddress
		val openSet = mutable.Set(start)
		val cameFrom = mutable.Map.empty[Address, Address]
		val gScore = mutable.Map(start -> 0).withDefaultValue(Int.MaxValue)

		def h(n: Address) = n.distTo(goal)

		val fScore = mutable.Map(start -> h(start))

		@tailrec def searchLoop(): Unit = {
			if (openSet.isEmpty) throw new NoSuchElementException("Search failed")

			val current = openSet.minBy(fScore)
			if (current == goal) {
				// exit!
			} else {
				openSet.remove(current)
				for (neighbor <- topo.neighborsOf(current)) {
					val tentativeScore = gScore(current) + d(current, neighbor)
					if (tentativeScore < gScore(neighbor)) {
						cameFrom(neighbor) = current
						gScore(neighbor) = tentativeScore
						fScore(neighbor) = tentativeScore + h(neighbor)
						openSet.add(neighbor)
					}
				}
				searchLoop()
			}
		}

		searchLoop()

		def reconstructPath(n: Address, accum: List[Address]): List[Address] = {
			cameFrom.get(n) match {
				case None =>
					require(n == start)
					accum
				case Some(prev) =>
					reconstructPath(prev, prev :: accum)
			}
		}

		reconstructPath(goal, Nil)
	}

	// Part 2: do a BFS backwards from the "goal" node to any node with elevation 0 ('a')
	def reverseBfs(topo: TopoMap) = {
		val queue = mutable.Queue.empty[Address]
		val parents = mutable.Map(topo.goalAddress -> topo.goalAddress)

		@tailrec def loop(here: Address): Address = {
			if (topo(here).value == 0) here
			else {
				topo.inverseNeighborsOf(here).filterNot(parents.contains).foreach { n =>
					parents(n) = here
					queue.enqueue(n)
				}
				if (queue.nonEmpty) loop(queue.dequeue())
				else throw new NoSuchElementException("search failed")
			}
		}

		val result = loop(topo.goalAddress)

		@tailrec def reconstructPath(here: Address, accum: List[Address]): List[Address] = {
			if (here == topo.goalAddress) accum
			else reconstructPath(parents(here), here :: accum)
		}

		reconstructPath(result, Nil)
	}

	def part1(): String = aStar(topoMap, (_, _) => 1).length.toString
	def part2(): String = reverseBfs(topoMap).length.toString

}
