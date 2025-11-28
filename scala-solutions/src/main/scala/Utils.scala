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

	extension [A](it: IterableOnce[A])
		def slidingPairs: Iterator[(A, A)] = it.iterator.sliding(2).flatMap(AsTuple2.unapply)

	extension [A, B](coll: Iterable[(A, B)])(using A: Numeric[A], B: Numeric[B])
		@targetName("sumTuplesIterable")
		def sumTuples: (A, B) = coll.iterator.sumTuples

	extension [A, B](it: Iterator[(A, B)])(using A: Numeric[A], B: Numeric[B])
		@targetName("sumTuplesIterator")
		def sumTuples: (A, B) = it.foldLeft((A.zero, B.zero)) { case ((accA, accB), (a, b)) =>
			(A.plus(accA, a), B.plus(accB, b))
		}

	trait StringDecoder[A]:
		def unapply(raw: String): Option[A]
		def typeName: String
		def apply(raw: String): A = unapply(raw).getOrElse {
			throw new IllegalArgumentException(s"Invalid $typeName: '$raw'")
		}
	object StringDecoder:
		def apply[A](_typeName: String, f: PartialFunction[String, A]): StringDecoder[A] =
			new StringDecoder[A]:
				val typeName: String = _typeName
				def unapply(raw: String): Option[A] = f.unapply(raw)

	case class Reverse[A](value: A)
	object Reverse:
		given [A](using ord: Ordering[A]): Ordering[Reverse[A]] with
			def compare(x: Reverse[A], y: Reverse[A]): Int =
				ord.compare(y.value, x.value)

	def memoize[A, B](f: A => B): A => B = {
		val cache = scala.collection.mutable.Map.empty[A, B]
		a => cache.getOrElseUpdate(a, f(a))
	}
}
