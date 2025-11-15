package io.dylemma.aoc

object Utils {
	object AsInt {
		def unapply(s: String): Option[Int] = {
			try {
				Some(s.toInt)
			} catch {
				case _: NumberFormatException => None
			}
		}
	}
}
