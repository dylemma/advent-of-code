package io.dylemma.aoc
package solutions.y2024

import Utils.{ AsTuple2, StringDecoder }

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.syntax.foldable.*

object Solution24 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {
		// parse the input file to get the initial signals and list of operations
		val (initialSignals, operations) = parseInput(input)

		// debug output of the parsed input
		if log.isDebugEnabled then {
			log.debug("Initial signals:")
			for (signal, bit) <- initialSignals.toList.sortBy(_._1) do
				log.debug(s"  $signal: ${ if (bit) 1 else 0 }")

			log.debug("Operations:")
			for op <- operations do
				log.debug(s"  $op")
		}

		val endState = solveSystem(initialSignals, operations)
		if log.isDebugEnabled then {
			log.debug("Final signals:")
			for (signal, bit) <- endState.toList.sortBy(_._1) do
				log.debug(s"  $signal: ${ if (bit) 1 else 0 }")
		}

		val part1Result = interpretZSignals(endState)
		log.info(s"Part 1: interpreted Z signals as value $part1Result")
	}

	opaque type Signal = String
	extension (self: Signal)
		def zRank: Option[Int] =
			if self.startsWith("z") then
				Some(self.stripPrefix("z").toInt)
			else
				None

	enum Operator(f: (Boolean, Boolean) => Boolean):
		case AND extends Operator(_ & _)
		case OR extends Operator(_ | _)
		case XOR extends Operator(_ ^ _)
		def apply(a: Boolean, b: Boolean): Boolean = f(a, b)

	case class Operation(l: Signal, r: Signal, op: Operator, out: Signal):
		override def toString = s"$l $op $r -> $out"

	// get a tuple of the initial signal bits and the list of operations
	object parseInput {
		def apply(input: String): (Map[Signal, Boolean], List[Operation]) = input
			.linesIterator
			.foldLeft((false, Map.empty[Signal, Boolean], List.empty[Operation])) {
				case ((false, signalMap, ops), line) if line.isBlank =>
					// switch to parsing ops
					(true, signalMap, ops)

				case ((false, signalMap, ops), InitLine(signal, bit)) =>
					(false, signalMap.updated(signal, bit), ops)

				case ((true, signalMap, ops), OperationLine(op)) =>
					(true, signalMap, op :: ops)

				case ((expectsOps, _, _), line) =>
					val expected = if (expectsOps) "operation line" else "initializer line"
					throw new IllegalArgumentException(s"Invalid $expected: '$line'")
			}
			.pipe { case (_, signalMap, ops) =>
				signalMap -> ops.reverse
			}

		private val InitLinePattern = raw"(\w+): (\d)".r.anchored
		private val InitLine = StringDecoder("initializer line", {
			case InitLinePattern(signal, num) => (signal: Signal) -> (num == "1")
		})

		private val OperationPattern =  raw"(\w+) (\w+) (\w+) -> (\w+)".r.anchored
		private val OperationLine = StringDecoder("operation", {
			case OperationPattern(l, OperatorString(op), r, out) => Operation(l, r, op, out)
		})

		private val OperatorString = StringDecoder("operator", {
			case "AND" => Operator.AND
			case "OR"  => Operator.OR
			case "XOR" => Operator.XOR
		})
	}

	def solveSystem(inits: Map[Signal, Boolean], ops: List[Operation]): Map[Signal, Boolean] = {
		@tailrec
		def step(currentState: Map[Signal, Boolean], pendingOps: List[Operation]): Map[Signal, Boolean] = {
			val (newState, remainingOps) = pendingOps.foldLeft(currentState -> List.empty[Operation]) {
				case ((state, failedOps), op @ Operation(l, r, f, out)) =>
					val ready = state.contains(l) && state.contains(r)
					if ready then
						val result = f(state(l), state(r))
						state.updated(out, result) -> failedOps
					else
						state -> (op :: failedOps)
			}
			if remainingOps.isEmpty then
				newState
			else
				require(remainingOps.size < pendingOps.size, "No progress made in step; circular dependency?")
				step(newState, remainingOps)
		}
		step(inits, ops)
	}

	def interpretZSignals(state: Map[Signal, Boolean]): Long = {
		(for {
			(signal, bit) <- state.view
			zRank <- signal.zRank
		} yield {
			val bitValue = if (bit) 1L else 0L
			bitValue << zRank
		}).sum
	}
}
