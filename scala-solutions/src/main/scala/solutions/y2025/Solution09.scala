package io.dylemma.aoc
package solutions.y2025

import Utils.*

import java.io.{ BufferedOutputStream, File, FileOutputStream, PrintWriter }
import scala.collection.immutable.SortedSet
import scala.util.chaining.*

object Solution09 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {
		val (shouldRunPart1, shouldRunPart2) = partsToRun(args)
		val shouldRenderToConsole = args.contains("render")
		val shouldRenderToSvg = args.contains("svg")

		val redTiles: List[Pos] = input.linesIterator.map { line =>
			line.split(',') match {
				case Array(AsLong(x), AsLong(y)) => Pos(x, y)
				case other => throw new IllegalArgumentException(s"Invalid tile line: '$line'")
			}
		}.toList

		// show the grid to check our "contains" logic
		if (shouldRenderToConsole) {
			drawConsole(redTiles)
		}

		if (shouldRunPart1) {
			val largestRect = redTiles.distinctPairs.map(Rect.apply.tupled).maxBy(_.area)
			log.debug(s"Biggest rect: $largestRect")
			log.info(s"Part 1: ${ largestRect.area }")
		}

		if (shouldRunPart2) {

			val shape = Shape.buildFrom(redTiles)

			

			val bestRect = redTiles.distinctPairs.map(Rect.apply.tupled)
				.filter (shape.containsRect)
				.maxBy(_.area)

			log.info(s"Part 2: $bestRect = ${ bestRect.area }")


			if (shouldRenderToSvg) {
				drawSvg(redTiles, bestRect)
			}
		}
	}

	/** Renders the puzzle inputs to the console.
	  * 
	  * Since the real puzzle input has huge dimensions, this method copes by scaling down
	  * the coordinates to a more manageable size. Rather than a linear scale, it picks out
	  * all the distinct X and Y values, and maps them onto a dense range starting at 1.
	  * The effect is a sort of "squish" of the circular shape described by the puzzle input,
	  * into more of a diamond shape, but for the sake of getting a quick intuition of the
	  * general shape, this is sufficient.
	  * 
	  * The rendering uses [[Shape.isOnOutline]] and [[Shape.isInside]] to distinguish
	  * border and inner tiles respectively.
	  * 
	  * @param actualRedTiles The parsed puzzle inputs, without any transformation applied
	  */
	def drawConsole(actualRedTiles: List[Pos]) = {
		val redTiles =
			if (actualRedTiles.size > 50) {
				val xMapping = actualRedTiles.view.map(_.x).to(SortedSet).view.zipWithIndex.map { case (x, idx) => x -> (idx + 1L) }.toMap
				val yMapping = actualRedTiles.view.map(_.y).to(SortedSet).view.zipWithIndex.map { case (y, idx) => y -> (idx + 1L) }.toMap
				actualRedTiles.map { pos =>
					Pos(xMapping(pos.x), yMapping(pos.y))
				}
			} else {
				actualRedTiles
			}

		val shape = Shape.buildFrom(redTiles)

		val maxX = redTiles.map(_.x).max + 2
		val maxY = redTiles.map(_.y).max + 1
		for (y <- 0L to maxY) {
			val line = (0L to maxX).map { x =>
				val pos = Pos(x, y)
				if (redTiles.contains(pos)) '#'
				else if (shape.isOnOutline(pos)) 'O'
				else if (shape.contains(pos)) 'X'
				else '.'
			}.mkString
			println(line)
		}
	}

	/** Renders the shape described by the puzzle input, and the part 2 solution rectangle, to an SVG file. 
	  * 
	  * No transformation is applied to the tile positions (unlike [[drawConsole]]), but due to the dimensions,
	  * the renderer needs to pick a different stroke width depending on whether it's the example input or the real input.
	  */
	def drawSvg(redTiles: List[Pos], bestRect: Rect) = {
		val svgFile = new File("./output/solution09_shape.svg")
		val out = new PrintWriter(new BufferedOutputStream(new FileOutputStream(svgFile)))

		val maxX = redTiles.map(_.x).max + 2
		val maxY = redTiles.map(_.y).max + 1
		
		// The example dimensions are small, so we need a sub-integer stroke width so it doesn't look huge when scaled up to the viewport.
		// Conversely, the puzzle input dimensions are huge, so we need a very large stroke width to make it visible at all when scaled down to the viewport.
		val strokeWidth = if (maxX < 100) 0.1 else 500

		try {
			out.println(s"""<svg width="600" height="600" viewBox="0 0 $maxX $maxY" xmlns="http://www.w3.org/2000/svg" version="1.1">""")
			out.print(s"<path stroke=\"green\" stroke-width=\"$strokeWidth\" fill=\"transparent\" ")

			out.print("d=\"")

			out.print(s"M${ redTiles.head.x } ${ redTiles.head.y } ")

			for (Pos(x, y) <- redTiles.tail.iterator.concat(Iterator.single(redTiles.head))) {
				out.print(s"L$x $y ")
			}

			out.print("\"/>")

			out.println("<path d=\"")
			out.print(s"M${ bestRect.cornerA.x } ${ bestRect.cornerA.y }")
			out.print(s"L${ bestRect.cornerB.x } ${ bestRect.cornerA.y }")
			out.print(s"L${ bestRect.cornerB.x } ${ bestRect.cornerB.y }")
			out.print(s"L${ bestRect.cornerA.x } ${ bestRect.cornerB.y }")
			out.print(" Z\" ")
			out.print(s" stroke=\"red\" stroke-width=\"$strokeWidth\" fill=\"transparent\" />")

			out.println("</svg>")
		} finally {
			out.flush()
			out.close()
		}
		log.info(s"Drew shape SVG to ${ svgFile.getCanonicalPath }")
	}

	case class Pos(x: Long, y: Long)
	case class Rect(cornerA: Pos, cornerB: Pos) {
		def area = {
			val width = (cornerB.x - cornerA.x).abs + 1
			val height = (cornerB.y - cornerA.y).abs + 1
			width * height
		}
	}

	def verticalSegment(x: Long, yA: Long, yB: Long): VerticalSegment = {
		if (yB < yA)
			VerticalSegment(x, yMin = yB, yMax = yA, isBackwards = true)
		else
			VerticalSegment(x, yMin = yA, yMax = yB, isBackwards = false)
	}
	case class VerticalSegment(x: Long, yMin: Long, yMax: Long, isBackwards: Boolean) {
		override def toString = s"Vertical(x=$x, y=[$yMin ${if isBackwards then "<<" else ">>"} $yMax])"

		// check if a point is on this segment
		def contains(pos: Pos): Boolean = {
			pos.x == x && pos.y >= yMin && pos.y <= yMax
		}

		// intersection test, excluding endpoints
		def isStrictlyCrossedBy(horizontal: HorizontalSegment): Boolean = {
			horizontal.y > yMin && horizontal.y < yMax &&
				x > horizontal.xMin && x < horizontal.xMax
		}
	}

	def horizontalSegment(y: Long, xA: Long, xB: Long): HorizontalSegment = {
		if (xB < xA)
			HorizontalSegment(y, xMin = xB, xMax = xA, isBackwards = true)
		else
			HorizontalSegment(y, xMin = xA, xMax = xB, isBackwards = false)
	}

	case class HorizontalSegment(y: Long, xMin: Long, xMax: Long, isBackwards: Boolean) {
		override def toString = s"Horizontal(y=$y, x=[$xMin ${if isBackwards then "<<" else ">>"} $xMax])"
		def contains(pos: Pos): Boolean = {
			pos.y == y && pos.x >= xMin && pos.x <= xMax
		}
		def isStrictlyCrossedBy(vertical: VerticalSegment): Boolean = {
			vertical.x > xMin && vertical.x < xMax &&
				y > vertical.yMin && y < vertical.yMax
		}
	}

	opaque type XValue = Long
	opaque type YValue = Long

	/** "Inside" helper for the puzzle.
	  * 
	  * Since the input describes the perimeter of a shape using axis-aligned segments,
	  * our "inside" test for a point uses raycasting to count the number of outline
	  * segments crossed by a ray extending rightwards to the point, and downwards to the point.
	  * 
	  * There is special handling for when the ray crosses exactly at the endpoint of a segment,
	  * to avoid double-counting (or never-counting) such cases. 
	  * 
	  * A rectangle is considered "inside" the shape as long as all four corners are inside,
	  * and none of its edges cross any of the shape's outline segments.
	  */
	object Shape {
		def buildFrom(redTiles: List[Pos]): Shape = {
			val (horizontalSegments, verticalSegments) = redTiles
				.iterator
				.concat { Iterator.single(redTiles.head) }
				.slidingPairs
				.foldLeft(Vector.empty[HorizontalSegment] -> Vector.empty[VerticalSegment]) { case ((hSegs, vSegs), (p1, p2)) =>
					val Seq(xMin, xMax) = Seq(p1.x, p2.x).sorted
					val Seq(yMin, yMax) = Seq(p1.y, p2.y).sorted
					if (xMin == xMax) (hSegs, vSegs :+ verticalSegment(x = xMin, yA = p1.y, yB = p2.y))
					else if (yMin == yMax) (hSegs :+ horizontalSegment(y = yMin, xA = p1.x, xB = p2.x), vSegs)
					else throw new IllegalArgumentException(s"Non-axis-aligned segment between $p1 and $p2")
				}

			Shape(
				horizontalSegments.groupBy(_.y),
				verticalSegments.groupBy(_.x),
			)
		}
	}

	case class Shape(
		horizontalSegments: Map[YValue, Vector[HorizontalSegment]],
		verticalSegments: Map[XValue, Vector[VerticalSegment]],
	) {
		lazy val verticalSegmentsOrdered: List[(XValue, Vector[VerticalSegment])] =
			verticalSegments.toList.sortBy(_._1)
		lazy val horizontalSegmentsOrdered: List[(YValue, Vector[HorizontalSegment])] =
			horizontalSegments.toList.sortBy(_._1)

		def isOnOutline(pos: Pos): Boolean = {
			horizontalSegments.get(pos.y).exists { segs =>
				segs.exists { _.contains(pos) }
			} || verticalSegments.get(pos.x).exists { segs =>
				segs.exists { _.contains(pos) }
			}
		}

		def isInside(pos: Pos): Boolean = {
			// count the number of vertical segments that would be crossed
			// by a ray from `(x=0, y=pos.y)` to `pos`
			val raycastH = HorizontalSegment(y = pos.y, xMin = -1L, xMax = pos.x, isBackwards = false)
			val raycastV = VerticalSegment(x = pos.x, yMin = -1L, yMax = pos.y, isBackwards = false)

			val hHitsAlt = countActualHCrossing(raycastH)
			val vHitsAlt = countActualVCrossing(raycastV)

			(hHitsAlt % 2 == 1) && (vHitsAlt % 2 == 1)
		}

		def countIntersections(raycastH: HorizontalSegment): Int = {
			verticalSegments.count { case (x, verticals) =>
				raycastH.xMin < x &&
					raycastH.xMax > x &&
					verticals.exists(_.isStrictlyCrossedBy(raycastH))
			}
		}

		def collectIntersections(raycastH: HorizontalSegment): Vector[VerticalSegment] = {
			verticalSegments.view.flatMap { case (x, verticals) =>
				// check that the horizontal raycast traverses across the vertical plane
				// that this group of segments exist on
				if (raycastH.xMin < x && raycastH.xMax > x) {
					verticals.filter { seg =>
						// seg is a vertical segment at x, and the x crossing was already checked,
						// so now we need to make sure the raycast's Y is within the vertical segment's Y range
						raycastH.y >= seg.yMin && raycastH.y <= seg.yMax
					}
				} else {
					Vector.empty
				}
			}.toVector
		}

		def collectIntersections(raycastV: VerticalSegment): Vector[HorizontalSegment] = {
			horizontalSegments.view.flatMap { case (y, horizontals) =>
				// check that the vertical raycast traverses across the horizontal plane
				// that this group of segments exist on
				if (raycastV.yMin < y && raycastV.yMax > y) {
					horizontals.filter { seg =>
						// seg is a horizontal segment at y, and the y crossing was already checked,
						// so now we need to make sure the raycast's X is within the horizontal segment's X range
						raycastV.x >= seg.xMin && raycastV.x <= seg.xMax
					}
				} else {
					Vector.empty
				}
			}.toVector
		}

		def countIntersections(raycastV: VerticalSegment): Int = {
			horizontalSegments.count { case (y, horizontals) =>
				raycastV.yMin < y &&
					raycastV.yMax > y &&
					horizontals.exists(_.isStrictlyCrossedBy(raycastV))
			}
		}

		def countActualHCrossing(ray: HorizontalSegment) = {
			// prevEndpointCrossing = Option(Left(prevSegment.yMax) | Right(prevSegment.yMin))
			def loop(itr: Iterator[VerticalSegment], count: Int, prevEndpointCrossing: Option[Either[Long, Long]]): Int = itr.nextOption() match {
				case None => count
				case Some(seg) =>
					if (ray.y == seg.yMin) {
						val thisEndpointCrossing = Some(Right(seg.yMin))
						// normally this wouldn't count as a crossing, but if the previous segment was also
						// touched just at its yMax, we'll count the pair as having been crossed once
						// (is previous segment's max (Left) equal to this segment's min?)
						if (prevEndpointCrossing.contains(Left(seg.yMin))) {
							loop(itr, count + 1, thisEndpointCrossing)
						} else {
							loop(itr, count, thisEndpointCrossing)
						}
					} else if (ray.y == seg.yMax) {
						val thisEndpointCrossing = Some(Left(seg.yMax))
						// normally this wouldn't count as a crossing, but if the previous segment was also
						// touched just at its yMin, we'll count the pair as having been crossed once
						// (is previous segment's min (Right) equal to this segment's max?)
						if (prevEndpointCrossing.contains(Right(seg.yMax))) {
							loop(itr, count + 1, thisEndpointCrossing)
						} else {
							loop(itr, count, thisEndpointCrossing)
						}
					} else {
						// the segment was cleanly crossed through its midsection;
						// count this crossing, and consider any previous "endpoint crossing"
						// as having just clipped through the edge, and not as a crossing
						loop(itr, count + 1, None)
					}
			}

			loop(collectIntersections(ray).sortBy(_.x).iterator, 0, None)
		}

		def countActualVCrossing(ray: VerticalSegment) = {
			// prevEndpointCrossing = Option(Left(prevSegment.xMax) | Right(prevSegment.xMin))
			def loop(itr: Iterator[HorizontalSegment], count: Int, prevEndpointCrossing: Option[Either[Long, Long]]): Int = itr.nextOption() match {
				case None => count
				case Some(seg) =>
					if (ray.x == seg.xMin) {
						val thisEndpointCrossing = Some(Right(seg.xMin))
						// normally this wouldn't count as a crossing, but if the previous segment was also
						// touched just at its xMax, we'll count the pair as having been crossed once
						if (prevEndpointCrossing.contains(Left(seg.xMin))) {
							loop(itr, count + 1, thisEndpointCrossing)
						} else {
							loop(itr, count, thisEndpointCrossing)
						}
					} else if (ray.x == seg.xMax) {
						val thisEndpointCrossing = Some(Left(seg.xMax))
						// normally this wouldn't count as a crossing, but if the previous segment was also
						// touched just at its xMin, we'll count the pair as having been crossed once
						if (prevEndpointCrossing.contains(Right(seg.xMax))) {
							loop(itr, count + 1, thisEndpointCrossing)
						} else {
							loop(itr, count, thisEndpointCrossing)
						}
					} else {
						// the segment was cleanly crossed through its midsection;
						// count this crossing, and consider any previous "endpoint crossing"
						// as having just clipped through the edge, and not as a crossing
						loop(itr, count + 1, None)
					}
			}

			loop(collectIntersections(ray).sortBy(_.y).iterator, 0, None)
		}

		def contains(pos: Pos): Boolean = {
			isOnOutline(pos) || isInside(pos)
		}

		def containsRect(rect: Rect): Boolean = {
			val Seq(xMin, xMax) = Seq(rect.cornerA.x, rect.cornerB.x).sorted
			val Seq(yMin, yMax) = Seq(rect.cornerA.y, rect.cornerB.y).sorted

			val leftVertical = verticalSegment(x = xMin, yMin, yMax)
			val rightVertical = verticalSegment(x = xMax, yMin, yMax)
			val topHorizontal = horizontalSegment(y = yMin, xMin, xMax)
			val bottomHorizontal = horizontalSegment(y = yMax, xMin, xMax)

			// all four corners must be contained in the shape,
			// and none of the edges must cross any of the shape's outline
			val corners = Seq(
				Pos(xMin, yMin),
				Pos(xMin, yMax),
				Pos(xMax, yMin),
				Pos(xMax, yMax),
			)

			corners.forall(contains) &&
				countIntersections(leftVertical) == 0 &&
				countIntersections(rightVertical) == 0 &&
				countIntersections(topHorizontal) == 0 &&
				countIntersections(bottomHorizontal) == 0
		}
	}
}
