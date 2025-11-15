package io.dylemma.aoc

import Utils.AsInt

import scala.annotation.tailrec

import ch.qos.logback.classic.Level

case class CLI(
	day: Int,
	isExample: Boolean,
	extraArgs: List[String],
)

object CLI {
	def parse(args: Array[String]): CLI = readArgs(args.toList, ParseState())

	private case class ParseState(
		isExample: Boolean = false,
	)

	@tailrec
	private def readArgs(args: List[String], accum: ParseState): CLI = args match {
		case Nil =>
			println("Error: No day specified. Use --help for usage.")
			sys.exit(-1)

		case "--help" :: _ | "-h" :: _ =>
			println(
				"""Usage: sbt run [options] DAY [...extraArgs]
				|Options:
				|--help, -h        Show this help message
				|--debug           Enable debug logging
				|--example         Use example inputs
				""".stripMargin,
			)
			sys.exit(-1)

		case "--debug" :: tail =>
			Logging.setRootLevel(Level.DEBUG)
			readArgs(tail, accum)

		case "--example" :: tail =>
			readArgs(tail, accum.copy(isExample = true))

		case AsInt(day) :: tail =>
			CLI(day, accum.isExample, tail)

		case other :: _ =>
			println(s"Error: Unrecognized argument '$other'. Use --help for usage.")
			sys.exit(-1)
	}
}
