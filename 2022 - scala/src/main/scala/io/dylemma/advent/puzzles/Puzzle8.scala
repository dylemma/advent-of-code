package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Puzzle

import scala.annotation.tailrec

object Puzzle8 extends Puzzle {
	val treeHeightsYX = inputLines.map(_.iterator.map(String.valueOf(_).toInt).toVector)
	val gridHeight = treeHeightsYX.length
	val gridWidth = treeHeightsYX.head.length

	def heightAt(x: Int, y: Int) = treeHeightsYX(y)(x)
	def visible(x: Int, y: Int) = {
		val myHeight = heightAt(x, y)
		@inline def visibleFromTop = (0 until y).forall(heightAt(x, _) < myHeight)
		@inline def visibleFromBottom = (y + 1 until gridHeight).forall(heightAt(x, _) < myHeight)
		@inline def visibleFromLeft = (0 until x).forall(heightAt(_, y) < myHeight)
		@inline def visibleFromRight = (x + 1 until gridWidth).forall(heightAt(_, y) < myHeight)
		visibleFromTop || visibleFromBottom || visibleFromLeft || visibleFromRight
	}
	@tailrec def countVisible(myHeight: Int, heights: Iterator[Int], count: Int): Int = {
		// basically `takeWhile(height < myHeight)` but with a +1 to count the failing element
		if (heights.hasNext) {
			val h = heights.next()
			if (h < myHeight) countVisible(myHeight, heights, count + 1)
			else count + 1
		} else {
			count
		}
	}
	def scenicScore(x: Int, y: Int) = {
		val myHeight = heightAt(x, y)
		val topDist = countVisible(myHeight, (y - 1 to 0 by -1).iterator.map(heightAt(x, _)), 0)
		val bottomDist = countVisible(myHeight, (y + 1 until gridHeight).iterator.map(heightAt(x, _)), 0)
		val leftDist = countVisible(myHeight, (x - 1 to 0 by -1).iterator.map(heightAt(_, y)), 0)
		val rightDist = countVisible(myHeight, (x + 1 until gridWidth).iterator.map(heightAt(_, y)), 0)
		topDist * bottomDist * leftDist * rightDist
	}

	def part1(): String = {
		val visibleTreesXY = for {
			y <- 0 until gridHeight
			x <- 0 until gridWidth
			if visible(x, y)
		} yield (x, y)

		visibleTreesXY.length.toString
	}

	def part2(): String = {
		val scenicScores = for {
			y <- 0 until gridHeight
			x <- 0 until gridWidth
		} yield scenicScore(x, y)
		scenicScores.max.toString
	}
}
