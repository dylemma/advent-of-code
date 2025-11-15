package io.dylemma.aoc

import ch.qos.logback.classic.{ Level, LoggerContext }
import org.slf4j.{ Logger, LoggerFactory }

trait Logging {
	protected lazy val log: Logger = LoggerFactory.getLogger(getClass)
}

object Logging {
	def setRootLevel(level: Level): Unit = {
		val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
		ctx.getLogger(Logger.ROOT_LOGGER_NAME).setLevel(level)
	}
}
