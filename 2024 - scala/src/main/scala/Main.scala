package io.dylemma.aoc

object Main extends Logging {

	def main(args: Array[String]): Unit = {
		val CLI(day, isExample, extraArgs) = CLI.parse(args)

		val input: String =
			if isExample
			then Inputs.getExampleInput(day)
			else Inputs.getInput(day)

		val solution: Solution = day match {
			case 21 => solutions.Solution21
			case 22 => solutions.Solution22
			case 23 => solutions.Solution23
			case 24 => solutions.Solution24
			case _ =>
				log.warn(s"No solution implemented for day $day")
				sys.exit(2)
		}
		
		solution.run(input, extraArgs)
	}
}
