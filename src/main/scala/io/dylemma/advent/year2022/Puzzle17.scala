package io.dylemma.advent
package year2022

import scala.annotation.tailrec

object Puzzle17 extends util.Puzzle {

	// conceptually a Vector[Boolean], represented as a bit set, where vec(i) == bits & (i^2)
	case class TowerRow(tiles: Int) {
		def hasRockAt(x: Int) = (tiles & (1 << x)) != 0
		def collides(other: TowerRow) = (tiles & other.tiles) != 0
		def toDebugString = (0 until 7).iterator.map(hasRockAt).map { if (_) '#' else '.' }.mkString("|", "", "|")
		def canShiftLeft = !hasRockAt(0)
		def canShiftRight = !hasRockAt(6)
		def shiftLeft = if (canShiftLeft) TowerRow(tiles >> 1) else this
		def shiftRight = if (canShiftRight) TowerRow(tiles << 1) else this
		def merge(other: TowerRow) = TowerRow(tiles | other.tiles)
	}
	object TowerRow {
		def apply(range: Range): TowerRow = TowerRow(range.iterator.map(1 << _).sum)
	}

	// rows(0) is the bottom of the shape
	case class Shape(rows: Vector[TowerRow]) {
		def toDebugString = rows.reverseIterator.map(_.toDebugString).mkString("\n")
		def shiftLeft = if (rows.forall(_.canShiftLeft)) Shape(rows.map(_.shiftLeft)) else this
		def shiftRight = if (rows.forall(_.canShiftRight)) Shape(rows.map(_.shiftRight)) else this
	}

	case class Tower(rows: Vector[TowerRow]) {
		def maxHeight = rows.length
		def intersects(shape: Shape, offsetY: Int) = {
			if (offsetY < 0) true
			else shape.rows.view.zipWithIndex.exists { case (shapeRow, i) =>
				val y = i + offsetY
				if (y >= rows.length) {
					// shape is above the tower
					false
				} else {
					val towerRow = rows(y)
					towerRow.collides(shapeRow)
				}
			}
		}
		def place(shape: Shape, offsetY: Int) = {
			val replacements = rows.view
				.slice(offsetY, offsetY + shape.rows.length)
				.zipAll(shape.rows, TowerRow(0), TowerRow(0))
				.map { case (a, b) => a.merge(b) }

			Tower(rows.patch(offsetY, replacements, shape.rows.length))
		}
		def toDebugString = {
			rows.reverseIterator.map(_.toDebugString).concat(Option("-" * 9)).mkString("\n")
		}
	}

	val gustPattern = inputLines.head.collect[Shape => Shape] {
		case '<' => _.shiftLeft
		case '>' => _.shiftRight
	}
	val gustQueue = LazyList.continually { gustPattern }.flatten

	val shapePattern = Vector(
		// ..@@@@.
		Shape(Vector(
			/* y=0 */ TowerRow(2 to 5),
		)),

		// ...@...
		// ..@@@..
		// ...@...
		Shape(Vector(
			/* y=0 */ TowerRow(3 to 3),
			/* y=1 */ TowerRow(2 to 4),
			/* y=2 */ TowerRow(3 to 3),
		)),

		// ....@..
		// ....@..
		// ..@@@..
		Shape(Vector(
			/* y=0 */ TowerRow(2 to 4),
			/* y=1 */ TowerRow(4 to 4),
			/* y=2 */ TowerRow(4 to 4),
		)),

		// ..@....
		// ..@....
		// ..@....
		// ..@....
		Shape(Vector.fill(4)(TowerRow(2 to 2))),

		// ..@@...
		// ..@@...
		Shape(Vector.fill(2)(TowerRow(2 to 3))),
	)

	val shapeQueue = LazyList.continually(shapePattern).flatten

//	case class State(falling: Shape, offsetY: Int, queue: LazyList[Shape], tower: Tower) {
//		def gustLeft = {
//			val atLeft = falling.shiftLeft
//			if (tower.intersects(atLeft, offsetY)) this
//			else State(atLeft, offsetY, queue, tower)
//		}
//		def gustRight = {
//			val atRight = falling.shiftRight
//			if (tower.intersects(atRight, offsetY)) this
//			else State(atRight, offsetY, queue, tower)
//		}
//		def fall = {
//			if (tower.intersects(falling, offsetY - 1)) {
//				// falling would cause an intersection, so the shape comes to rest at the current Y pos
//				// and we spawn a new shape 3 units above the new tower apex
//				val newTower = tower.place(falling, offsetY)
//				State(queue.head, newTower.maxHeight + 3, queue.tail, newTower)
//			} else {
//				State(falling, offsetY - 1, queue, tower)
//			}
//		}
//	}

	def simulate(numRocks: Int) = {
		val gusts = gustQueue.iterator
		shapeQueue.take(numRocks).foldLeft(Tower(Vector.empty)) { (tower, shape) =>
			@tailrec def loop(falling: Shape, offsetY: Int): Tower = {
				val shifted = gusts.next().apply(falling)
				val afterShift = if (tower.intersects(shifted, offsetY)) falling else shifted
				if (tower.intersects(afterShift, offsetY - 1)) {
					// falling would cause an intersection, so the shape comes to rest at the current Y pos
					// and we spawn a new shape 3 units above the new tower apex
					tower.place(afterShift, offsetY)
				} else {
					loop(afterShift, offsetY - 1)
				}
			}
			loop(shape, tower.maxHeight + 3)
		}
	}

	def part1(): String = {
		val tower = simulate(2022)
		println(tower.toDebugString)
		tower.maxHeight.toString
	}
	def part2(): String = "TODO"
}
