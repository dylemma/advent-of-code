package io.dylemma.aoc
package solutions.y2023

import scala.annotation.tailrec
import cats.syntax.option._

object Solution13 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {

		val pictures = parseInput(input.linesIterator)

		val part1Total = pictures
			.iterator
			.zipWithIndex
			.map { case (pic, index) =>
				log.debug(s"Picture ${ index + 1 }:")
				// scoreWith(pic, findNaturalSymmetry)
				scoreWith(pic, findSmudgedSymmetry)
			}
			.sum
		log.info(s"Part 1: $part1Total")
	}

	def parseInput(linesItr: Iterator[String]): List[Picture] = {
		val (group, tail) = linesItr.span(_.nonEmpty)
		group.toList match {
			case Nil => Nil
			case ne => Picture(ne) :: parseInput(tail.drop(1))
		}
	}

	case class Picture(lines: List[String]) {
		lazy val rotatedLines: List[String] = {
			val numCols = lines.head.length
			(0 until numCols).view.map { x =>
				lines.map(_ charAt x).mkString
			}.toList
		}

		def render: String = lines.mkString("\n")

		// get fancy debug output like what is shown in the puzzle description
		def renderWithHorizontalSymmetryMarker(beforeIndex: Int) = {
			val legend = Iterator.continually("123456789ABCDEF").flatten.take(lines.head.length).mkString
			val marker = " " * (beforeIndex - 1) + "><"
			((legend :: marker :: lines) ++ (marker :: legend :: Nil)).mkString("\n")
		}
		def renderWithVerticalSymmetryMarker(beforeIndex: Int) = {
			val legend = "123456789ABCDEF"
			lines.iterator.zipWithIndex.map { case (line, index) =>
				val marker =
					if (index == beforeIndex - 1) "v"
					else if (index == beforeIndex) "^"
					else " "
				val gutter = legend.charAt(index % legend.length)
				s"$gutter$marker$line$marker$gutter"
			}.mkString("\n")
		}
	}

	def scoreWith(pic: Picture, findSymmetry: List[String] => Option[Int]) = {
		findSymmetry(pic.lines) match {
			case Some(rowIndex) =>
				// vertical symmetry via reflection over a horizontal line
				log.debug(s"  Vertical symmetry found at index $rowIndex:\n${ pic.renderWithVerticalSymmetryMarker(rowIndex) }")
				100 * rowIndex
			case None =>
				findSymmetry(pic.rotatedLines) match {
					case Some(colIndex) =>
						// horizontal symmetry via reflection over a vertical line
						log.debug(s"  Horizontal symmetry found at index $colIndex:\n${ pic.renderWithHorizontalSymmetryMarker(colIndex) }")
						colIndex
					case None =>
						log.debug(s"  No symmetry found:\n${ pic.render }")
						0
				}
		}
	}

	case class ListZipper[A](leftReverse: List[A], index: Int, right: List[A]):
		def moveRight: Option[ListZipper[A]] = right match {
			case Nil => None
			case rHead :: rTail => Some(ListZipper(rHead :: leftReverse, index + 1, rTail))
		}
		def moveLeft: Option[ListZipper[A]] = leftReverse match {
			case Nil => None
			case lHead :: lTail => Some(ListZipper(lTail, index - 1, lHead :: right))
		}
		def hasSymmetry: Boolean = {
			leftReverse.zip(right) match {
				case Nil => false
				case ne => ne.forall { case (l, r) => l == r }
			}
		}

	extension (self: ListZipper[String]) {
		def hasSmudgedSymmetry: Boolean = {
			self.index > 0 &&
				compareWithSmudge(self.leftReverse, self.right, fixedSmudge = false)
		}
	}

	def findNaturalSymmetry(lines: List[String]) = {
		Iterator
			.iterate(ListZipper(Nil, 0, lines).some)(_.flatMap(_.moveRight))
			.takeWhile(_.isDefined)
			.flatten
			.find(_.hasSymmetry)
			.map(_.index)
	}

	@tailrec
	def compareWithSmudge(l: List[String], r: List[String], fixedSmudge: Boolean): Boolean = (l, r) match {
		case (lHead :: lTail, rHead :: rTail) =>
			if (lHead == rHead) {
				compareWithSmudge(lTail, rTail, fixedSmudge)
			} else if (fixedSmudge) {
				// already fixed one smudge to get here, can't fix more
				false
			} else {
				// see if there's exactly one character difference we can fix
				lHead.iterator.zip(rHead.iterator).count { case (lc, rc) => lc != rc } match {
					case 1 => compareWithSmudge(lTail, rTail, true)
					case _ => false
				}
			}
		case (Nil, _) | (_, Nil) =>
			// for part 2, "exactly one smudge" is in every picture, so if we got to the end
			// *without* fixing a smudge, then this wasn't the symmetry we were looking for
			fixedSmudge
	}

	def findSmudgedSymmetry(lines: List[String]) = {
		Iterator
			.iterate(ListZipper(Nil, 0, lines).some)(_.flatMap(_.moveRight))
			.takeWhile(_.isDefined)
			.flatten
			.find(_.hasSmudgedSymmetry)
			.map(_.index)
	}
}
