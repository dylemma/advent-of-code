package io.dylemma.aoc
package solutions.y2024

object Solution25 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {
		val parts = parseInput(input)
		val (locks, keys) = parts.partitionMap(identity)

		val combos = locks.view.flatMap: lock =>
			keys.view.map: key =>
				(lock, key)

		val fittingPairs = combos.count: (lock, key) =>
			key.fits(lock)

		log.info(s"Number of fitting lock/key pairs: $fittingPairs")
	}

	def parseInput(input: String) = {
		def step(tail: Iterator[String]): List[Either[Lock, Key]] = {
			val (leading, trailing) = tail.span(_.nonEmpty)
			val leadList = leading.toList
			if (leadList.isEmpty) Nil
			else parseChunk(leadList) :: step(trailing.drop(1))
		}
		step(input.linesIterator)
	}

	def parseChunk(lines: List[String]): Either[Lock, Key] = {
		val isLock = lines.head.forall(_ == '#')
		val otherLines = if isLock then lines.tail else lines.reverse.tail

		val heights = lines.head.indices.view.map { i =>
			otherLines.count(_.charAt(i) == '#')
		}.toVector

		if isLock then Left(Lock(heights)) else Right(Key(heights))
	}

	case class Lock(heights: Vector[Int]):
		override def toString = s"Lock(${heights mkString ","})"

	case class Key(heights: Vector[Int]):
		override def toString = s"Key(${heights mkString ","})"

		def fits(lock: Lock): Boolean = {
			heights.corresponds(lock.heights)(_ + _ < 6)
		}
}
