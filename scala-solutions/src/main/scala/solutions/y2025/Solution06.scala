package io.dylemma.aoc
package solutions.y2025

import Utils.AsLong

object Solution06 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)

		val allLines = input.linesIterator.toVector

		if (shouldRunPart1) {
			val grandTotal = solvePart1(allLines)
			log.info(s"Part 1 grand total: $grandTotal")
		}

		if (shouldRunPart2) {
			val grandTotal = solvePart2(allLines)
			log.info(s"Part 2: grand total: $grandTotal")
		}

	}

	enum Operator(val f: (Long, Long) => Long) {
		case Mult extends Operator(_ * _)
		case Add extends Operator(_ + _)
	}

	case class Operation(operator: Operator, operands: Vector[Long]) {
		def value = operands.reduce(operator.f)
	}

	def solvePart1(allLines: Vector[String]): Long = {
		val numericLines = allLines.init
		val operatorLine = allLines.last

		val numberRows = numericLines.map { line =>
			line.split("\\s+").view.filter(_.nonEmpty).map {
				case AsLong(num) => num
				case x => throw new IllegalArgumentException(s"Invalid number '$x' in line: '$line'")
			}.toVector
		}
		val operators = operatorLine.split("\\s+").view.map {
			case "*" => Operator.Mult
			case "+" => Operator.Add
			case s => throw new IllegalArgumentException(s"Unknown operator: '$s'")
		}.toVector

		log.debug("Parsed number rows:")
		for (row <- numberRows) {
			log.debug(s"  ${ row.mkString(", ") }")
		}
		log.debug(s"Parsed operators: $operators")

		val columns = operators.view.zipWithIndex.map { case (op, index) =>
			val operands = numberRows.view.map { _.apply(index) }.toVector
			Operation(op, operands)
		}
		log.debug("Constructed columns:")

		for (col <- columns) {
			log.debug(s"  $col}")
		}
		columns.iterator.map(_.value).sum
	}

	def solvePart2(allLines: Vector[String]): Long = {
		val numericLines = allLines.init
		val operatorLine = allLines.last

		val maxLineLength = numericLines.view.map(_.length).max
		val transposedLines = (0 until maxLineLength).view.map { i =>
			allLines.view.map(_.lift(i).getOrElse(' ')).mkString
		}.toList

		def go(toVisit: List[String]): List[Operation] = toVisit match {
			case Nil => Nil
			case head :: tail if head.isBlank => go(tail)
			case head :: tail =>
				val (groupLines, tailLines) = toVisit.span(!_.isBlank)
				require(groupLines.nonEmpty, s"Blank group found at $toVisit")
				val operator = groupLines.head.last match {
					case '*' => Operator.Mult
					case '+' => Operator.Add
					case c => throw new IllegalArgumentException(s"Unknown operator char: '$c'")
				}
				val operands = groupLines
					.view
					.map(_.trim.stripSuffix("*").stripSuffix("+").trim)
					.map {
						case AsLong(n) => n
						case s => throw new IllegalArgumentException(s"Invalid number string: '$s'")
					}
					.toVector

				Operation(operator, operands) :: go(tailLines)
		}

		go(transposedLines)
			.view
			.tapEach { op => log.debug(s"Transposed: $op") }
			.map(_.value)
			.sum
	}
}