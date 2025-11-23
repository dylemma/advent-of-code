package io.dylemma.aoc

import scopt.OParser

case class CliArgs(
	year: Option[Int] = None,
	day: Option[Int] = None,
	isDebugEnabled: Boolean = false,
	isExample: Boolean = false,
)

object CliArgs {
	def parse(args: Array[String]): Option[CliArgs] = {
		OParser.parse(CliArgs.parser, args, CliArgs())
	}

	private val parser = {
		val b = OParser.builder[CliArgs]
		OParser.sequence(
			b.programName("aoc"),
			b.head("Advent of Code solutions in Scala"),
			b.help('h', "help").text("Show this help message"),
			b.opt[Int]('y', "year")
				.action((y, c) => c.copy(year = Some(y)))
				.text("Year of the Advent of Code (20xx)"),
			b.opt[Int]('d', "day")
				.action((d, c) => c.copy(day = Some(d)))
				.text("Day of the Advent of Code (1-25)"),
			b.opt[Unit]('b', "debug")
				.action((_, c) => c.copy(isDebugEnabled = true))
				.text("Enable debug logging"),
			b.opt[Unit]('x', "example")
				.action((_, c) => c.copy(isExample = true))
				.text("Use example input data"),
		)
	}
}