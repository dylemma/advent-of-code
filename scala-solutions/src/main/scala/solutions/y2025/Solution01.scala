package io.dylemma.aoc
package solutions.y2025

object Solution01 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val dial = Dial(100)

		val p1Password = solvePart1(input, dial)
		log.info(s"The password (part 1) is $p1Password")

		val p2Password = solvePart2(input, dial)
		log.info(s"The password (part 2) is $p2Password")
	}

	case class Dial(length: Int):
		def left(current: Int, amount: Int) = {
			val maybeNegative = (current + (length - amount)) % length
			if (maybeNegative < 0) maybeNegative + length else maybeNegative
		}

		def right(current: Int, amount: Int) =
			(current + amount) % length

		// returns (newPosition, numPassesOrHitsOfZero)
		def rightWithIntermediates(current: Int, amount: Int): (Int, Int) = {
			val newPos = (current + amount) % length
			val passes = (current + amount) / length
			newPos -> passes
		}

		def leftWithIntermediates(current: Int, amount: Int): (Int, Int) = {
			val n = current - amount
			val newPos = left(current, amount)
			val passes =
				if n > 0 then 0
				else (n.abs / length) + 1
			val adjust = if current == 0 then -1 else 0
			newPos -> (passes + adjust).max(0)
		}

	def parseLine(line: String) = {
		if (line.startsWith("L")) {
			Left(line.drop(1).toInt)
		} else if (line.startsWith("R")) {
			Right(line.drop(1).toInt)
		} else {
			throw new IllegalArgumentException(s"Invalid line: '$line'")
		}
	}

	def solvePart1(input: String, dial: Dial): Int = {
		log.debug("The dial starts by pointing at 50.")
		input
			.linesIterator
			.map(parseLine)
			.scanLeft(50) {
				case (current, Left(amount)) =>
					val result = dial.left(current, amount)
					log.debug(s"The dial is rotated L$amount to point at $result.")
					result
				case (current, Right(amount)) =>
					val result = dial.right(current, amount)
					log.debug(s"The dial is rotated R$amount to point at $result.")
					result
			}
			.count(_ == 0)
	}

	def solvePart2(input: String, dial: Dial): Int = {
		log.debug("The dial starts by pointing at 50")

		// to get nice log output mimicking the puzzle text's example
		def pickLogSuffix(passes: Int, newPos: Int): String = {
			if (passes == 1 && newPos == 0) "."
			else if (passes == 1) "; during this rotation, it points at 0 once"
			else if (passes > 1) s"; during this rotation, it points at 0, $passes times"
			else "."
		}

		input
			.linesIterator
			.map(parseLine)
			.foldLeft(50 -> 0) {
				case ((current, totalPasses), Left(amount)) =>
					val (newPos, passes) = dial.leftWithIntermediates(current, amount)
					log.debug(s"The dial is rotated L$amount to point at $newPos${ pickLogSuffix(passes, newPos) }")
					newPos -> (totalPasses + passes)
				case ((current, totalPasses), Right(amount)) =>
					val (newPos, passes) = dial.rightWithIntermediates(current, amount)
					log.debug(s"The dial is rotated R$amount to point at $newPos${ pickLogSuffix(passes, newPos) }")
					newPos -> (totalPasses + passes)
			}
			._2
	}
}
