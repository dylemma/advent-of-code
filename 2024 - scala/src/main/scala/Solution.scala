package io.dylemma.aoc

trait Solution {
	def run(input: String, args: List[String]): Unit
}

object Solution {
	trait NoArgs extends Solution {
		final override def run(input: String, args: List[String]): Unit = {
			run(input)
		}
		def run(input: String): Unit
	}
}