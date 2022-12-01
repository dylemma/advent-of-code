package io.dylemma.advent.util

import cats.effect.Sync
import fs2.io._
import fs2.text.utf8

object Inputs {
	def lineStream[F[_]: Sync](resourceName: String) = {
		readClassResource[F, this.type](resourceName)
			.through(utf8.decode)
			.through(fs2.text.lines)
	}
}
