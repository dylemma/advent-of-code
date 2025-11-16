package io.dylemma.aoc

import scala.annotation.targetName

object Utils {
	object AsInt:
		def unapply(s: String): Option[Int] =
			try Some(s.toInt)
			catch case _: NumberFormatException => None

	object AsLong:
		def unapply(s: String): Option[Long] =
			try Some(s.toLong)
			catch case _: NumberFormatException => None

	object AsTuple2:
		def unapply[A](iterator: Iterable[A]) =
			val it = iterator.iterator
			for {
				a <- it.nextOption()
				b <- it.nextOption()
			} yield (a, b)

	extension [A, B](coll: Iterable[(A, B)])(using A: Numeric[A], B: Numeric[B])
		@targetName("sumTuplesIterable")
		def sumTuples: (A, B) = coll.iterator.sumTuples

	extension [A, B](it: Iterator[(A, B)])(using A: Numeric[A], B: Numeric[B])
		@targetName("sumTuplesIterator")
		def sumTuples: (A, B) = it.foldLeft((A.zero, B.zero)) { case ((accA, accB), (a, b)) =>
			(A.plus(accA, a), B.plus(accB, b))
		}

}
