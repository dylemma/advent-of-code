package io.dylemma.advent
package year2022

import util.TupleSyntax._

object Puzzle13 extends util.Puzzle {

	sealed trait Node
	case class Leaf(num: Int) extends Node
	case class Branch(nodes: List[Node]) extends Node

	object Node {
		/*
		 * Input Parsing
		 */
		def fromString(s: String) = {
			val (node, tail) = parseNode(s.toList)
			require(tail.isEmpty, s"Node parse failed with leftover characters: $tail")
			node
		}
		private def parseNode(chars: List[Char]): (Node, List[Char]) = chars match {
			case '[' :: tail => parseBranch(tail).map_1(Branch)
			case probablyNumbers => parseNum(probablyNumbers).map_1(Leaf)
		}
		private def parseBranch(chars: List[Char]): (List[Node], List[Char]) = chars match {
			case ']' :: tail => Nil -> tail
			case ',' :: tail => parseBranch(tail)
			case probablyNodes =>
				val (node, nextChars) = parseNode(probablyNodes)
				val (moreNodes, finalChars) = parseBranch(nextChars)
				(node :: moreNodes, finalChars)
		}
		private def parseNum(chars: List[Char]): (Int, List[Char]) = {
			val (digits, tail) = chars.span(_.isDigit)
			require(digits.nonEmpty, s"expected digits but got ${tail.headOption}")
			digits.mkString.toInt -> tail
		}

		/*
		 * Comparisons
		 */
		def isOrdered(l: Node, r: Node): Option[Boolean] = (l, r) match {
			case (Leaf(li), Leaf(ri)) => Option.when(li != ri) { li < ri }
			case (Branch(ln), Branch(rn)) =>
				ln.view.map(Option(_)).zipAll(rn.view.map(Option(_)), None, None).flatMap {
					case (Some(ln), Some(rn)) => isOrdered(ln, rn)
					case (Some(_), None) => Some(false) // right ran out first
					case (None, Some(_)) => Some(true) // left ran out first
					case (None, None) => None // equivalent
				}.headOption
			case (Leaf(_), Branch(_)) => isOrdered(Branch(l :: Nil), r)
			case (Branch(_), Leaf(_)) => isOrdered(l, Branch(r :: Nil))
		}
		implicit val nodeOrdering: Ordering[Node] = (x: Node, y: Node) => isOrdered(x, y) match {
			case Some(true) => -1
			case None => 0
			case Some(false) => 1
		}
	}

	// for `<` operator on Node
	import Node.nodeOrdering.mkOrderingOps

	def part1(): String = {
		inputLines
			.grouped(3)
			.map { lines => Node.fromString(lines(0)) -> Node.fromString(lines(1)) }
			.zipWithIndex
			.tapEach { case ((a, b), i) => println(s"== Pair ${i + 1} ==\n$a\n$b\n${a < b}\n") }
			.collect { case ((a, b), i) if a < b => i + 1 }
			.sum
			.toString
	}
	def part2(): String = {
		val dividers = Node.fromString("[[2]]") :: Node.fromString("[[6]]") :: Nil
		val sortedPackets = inputLines.view
			.filter(_.nonEmpty)
			.map(Node.fromString)
			.concat(dividers)
			.toArray
			.sortInPlace
		dividers.view.map(sortedPackets.indexOf).map(_ + 1).product.toString
	}
}
