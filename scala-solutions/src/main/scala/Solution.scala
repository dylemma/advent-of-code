package io.dylemma.aoc

trait Solution {
	def run(input: String, args: List[String]): Unit

	protected def partsToRun(args: List[String]): (Boolean, Boolean) = {
		val explicitlyPart1 = args.contains("part1") || args.contains("p1")
		val explicitlyPart2 = args.contains("part2") || args.contains("p2")
		val shouldRunPart1 = explicitlyPart1 || (!explicitlyPart1 && !explicitlyPart2)
		val shouldRunPart2 = explicitlyPart2 || (!explicitlyPart1 && !explicitlyPart2)
		(shouldRunPart1, shouldRunPart2)
	}
}

object Solution {
	trait NoArgs extends Solution {
		final override def run(input: String, args: List[String]): Unit = {
			run(input)
		}
		def run(input: String): Unit
	}
}