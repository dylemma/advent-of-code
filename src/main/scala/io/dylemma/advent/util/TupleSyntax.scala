package io.dylemma.advent.util

object TupleSyntax {
	implicit class Tuple2Ops[A, B](t: (A, B)) {
		def map_1[A1](f: A => A1) = (f(t._1), t._2)
		def map_2[B1](f: B => B1) = (t._1, f(t._2))
	}
}
