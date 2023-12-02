package io.dylemma.advent.util

import cats.Show
import cats.syntax.show._

case class OutLine[T](value: T) extends AnyVal
object OutLine {
	implicit def showOutLine[T: Show]: Show[OutLine[T]] = t => show"${ t.value }\n"
}
