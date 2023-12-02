package io.dylemma.advent.util

import scala.util.matching.Regex

object Matching {
	trait Matcher[+A] extends Function[String, A] {
		def unapply(s: String): Option[A]
		def apply(s: String) = unapply(s).getOrElse { throw new MatchError(s"Unable to match '$s'")}
		def map[B](f: A => B): Matcher[B] = (s: String) => unapply(s).map(f)
	}
	object Matcher {
		def confidently[A](f: String => A): Matcher[A] = s => Some(f(s))
	}

	object AsInt extends Matcher[Int] {
		def unapply(s: String) = {
			try Some(s.toInt)
			catch { case _: NumberFormatException => None }
		}
	}

	object AsLong extends Matcher[Long] {
		def unapply(s: String): Option[Long] = {
			try Some(s.toLong)
			catch { case _: NumberFormatException => None }
		}
	}

	object AsBigInt extends Matcher[BigInt] {
		def unapply(s: String) = {
			try Some(BigInt.apply(s))
			catch { case _: NumberFormatException => None }
		}
	}

	implicit class RegexMatcherOps(regex: Regex) {
		def asMatcher1: Matcher[String] = {
			case regex(cap) => Some(cap)
			case _ => None
		}
		def asMatcher2: Matcher[(String, String)] = {
			case regex(cap1, cap2) => Some((cap1, cap2))
			case _ => None
		}
		def intoMatcher[A](matcher: Matcher[A]): Matcher[A] = {
			case regex(matcher(a)) => Some(a)
			case _ => None
		}
	}
}
