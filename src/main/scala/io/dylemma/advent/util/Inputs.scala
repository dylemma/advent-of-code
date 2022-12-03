package io.dylemma.advent.util

import scala.collection.{ Factory, IterableFactory }

import cats.effect.{ Sync, SyncIO }
import fs2.io._
import fs2.text.utf8

object Inputs {
	def lineStream[F[_]: Sync](resourceName: String) = {
		readClassResource[F, this.type](resourceName)
			.through(utf8.decode)
			.through(fs2.text.lines)
	}

	def lines[CC[_]](resourceName: String)(implicit cc: Factory[String, CC[String]]) = {
		lineStream[SyncIO](resourceName)
			.compile
			.to(cc)
			.unsafeRunSync()
	}
}
