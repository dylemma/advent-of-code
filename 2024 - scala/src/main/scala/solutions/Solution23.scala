package io.dylemma.aoc
package solutions

import Utils.AsTuple2

import scala.annotation.tailrec
import scala.collection.View

object Solution23 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {

		val graph = input
			.linesIterator
			.map(parseLine)
			.tapEach { case (a, b) =>
				log.debug(s"Parsed line into: '$a' and '$b'")
			}
			.foldLeft(new Graph)(_ insert _)

		val numTClustersOf3: Int = graph
			.findClustersOfThree
			.iterator
			.distinct
			.filter { _.exists(_ startsWith "t") }
			.tapEach { c => log.debug(s"Found 't' cluster: ${showClique(c)}")}
			.size
		log.info(s"Found $numTClustersOf3 clusters of three nodes including at least one 't' node.")

		val biggestClique = bronKerbosch(graph)
			.iterator
			.tapEach { clique =>
				log.debug(s"Found clique: ${showClique(clique)}")
			}
			.maxBy(_.size)
		log.info(s"Largest clique is: ${showClique(biggestClique)}")

	}

	def parseLine(line: String): (String, String) = {
		AsTuple2.unapply(line.split('-')).getOrElse {
			throw new IllegalArgumentException(s"Werid line: '$line'")
		}
	}

	case class Graph(edges: Map[String, Set[String]]) {
		def this() = this(Map.empty)
		def insert(edge: (String, String)): Graph = Graph {
			val (a, b) = edge
			edges
				.updatedWith(a) {
					case None => Some(Set(b))
					case Some(neighbors) => Some(neighbors + b)
				}
				.updatedWith(b) {
					case None => Some(Set(a))
					case Some(neighbors) => Some(neighbors + a)
				}
		}

		// Helper for part 1; find any trio of nodes which are interconnected
		def findClustersOfThree: View[Set[String]] = {
			for {
				(a, neighborsOfA) <- edges.view
				b <- neighborsOfA.view
				c <- edges.getOrElse(b, Set.empty).view
				if c != a
				if neighborsOfA.contains(c)
			} yield Set(a, b, c)
		}
	}

	// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm (without pivot)
	def bronKerbosch(graph: Graph): List[Set[String]] = {
		val outputs = List.newBuilder[Set[String]]
		def recurse(r: Set[String], p: Set[String], x: Set[String], graph: Graph): Unit = {
			// if P and X are both empty then:
			//   report R as a maximal clique
			if p.isEmpty && x.isEmpty then outputs.addOne(r)

			// As described on Wikipedia, the exploration step is:
			//   for each vertex v in P do
			//       BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
			//       P := P \ {v}
			//       X := X ⋃ {v}
			// However, the iteration over P while modifying P is tricky with immutable sets,
			// so we instead use a tail-recursive helper to pass modified versions of P and X
			// to the next iteration of the loop
			@tailrec
			def explore(pp: Set[String], xx: Set[String]): Unit = {
				if (pp.nonEmpty) {
					val v = pp.head
					val `N(v)` = graph.edges.getOrElse(v, Set.empty)
					// BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
					recurse(r + v, pp.intersect(`N(v)`), xx.intersect(`N(v)`), graph)
					// P := P \ {v}
					// X := X ⋃ {v}
					explore(pp - v, xx + v)
				}
			}
			explore(p, x) // evaluate the `for each vertex v in P` loop
		}

		recurse(Set.empty, graph.edges.keySet, Set.empty, graph)
		outputs.result()
	}

	def showClique(clique: Set[String]): String = {
		s"[${ clique.toList.sorted.mkString(",") }]"
	}
}
