package io.dylemma.aoc

import java.net.http.HttpClient

object Http {
	lazy val client = HttpClient.newHttpClient()
}
