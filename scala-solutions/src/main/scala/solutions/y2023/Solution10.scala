package io.dylemma.aoc
package solutions.y2023

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.AnsiColor

object Solution10 extends Solution with Logging {
	def run(input: String, args: List[String]): Unit = {

		val maze = MazeInput(input.linesIterator.toVector)
		val distances: ComputedDistances = explorePipeLoop(maze)
		val insideTiles: Set[Address] = new Part2(maze, distances.visited).findInsideTiles

		if (args contains "draw") {
			// note: with SBT+Windows, the ANSI colors won't display properly by default.
			// Use the 'enable-fancy-output.ps1' script at the project root to fix that.
			Drawing.drawMaze(maze, distances, insideTiles)
		}

		log.info(s"Max distance from start: ${ distances.maxDistance }")
		log.info(s"Num inside tiles: ${ insideTiles.size }")
	}

	// Structural representation of the puzzle input
	case class MazeInput(rows: Vector[String]) {

		def charAt(addr: Address): Option[Char] = charAt(addr.x, addr.y)
		def charAt(x: Int, y: Int): Option[Char] = {
			rows.lift(y).flatMap(_.lift(x))
		}

		// find the address of the 'S' tile where the Pipe Loop starts
		def findStart: Address = {
			(for {
				(row, y) <- rows.iterator.zipWithIndex
				x = row.indexOf('S')
				if x >= 0
			} yield Address(x, y)).next()
		}

		// based on the tile at the given address, find what outgoing directions are possible
		def outDirections(here: Address): Set[Direction] = charAt(here) match {
			case None => Set.empty
			case Some('S') => Direction.all
			case Some(c) => getOutgoingConnections(c)
		}

		// by following `outDirections`, find neighboring tiles with a corresponding pipe connection
		def outNeighbors(here: Address): Iterator[Address] = {
			for {
				dir <- outDirections(here).iterator
				neighbor = here + dir
				if outDirections(neighbor).contains(dir.opposite)
			} yield neighbor
		}
	}

	// representation of the puzzle description of pipe connections
	val getOutgoingConnections: Char => Set[Direction] = Map(
		'|' -> Set(Direction.Up, Direction.Down),
		'-' -> Set(Direction.Left, Direction.Right),
		'7' -> Set(Direction.Left, Direction.Down),
		'J' -> Set(Direction.Left, Direction.Up),
		'F' -> Set(Direction.Right, Direction.Down),
		'L' -> Set(Direction.Right, Direction.Up),
	).withDefaultValue(Set.empty)

	object Drawing {
		// fancy utf-8 output is a lot easier to look at in the console vs the raw input characters
		private val pipeCharacters = Map(
			'-' -> '═',
			'|' -> '║',
			'7' -> '╗',
			'J' -> '╝',
			'F' -> '╔',
			'L' -> '╚',
		).withDefault(identity)

		// print the maze with ANSI colors for pipes and inside tiles
		def drawMaze(maze: MazeInput, distances: ComputedDistances, insideTiles: Set[Address]): Unit = {
			val maxDistance = distances.maxDistance

			// the pipe will mostly be green, but the start and 'farthest away' tiles
			// will be blue and red respectively, with a short gradient buffer to help
			// visually locate it when looking at the console output
			def pipeColor(distance: Distance) = {
				if (distance == 0) AnsiColor.BLUE
				else if (distance < 4) AnsiColor.CYAN
				else if (distance == maxDistance) AnsiColor.RED
				else if (distance > maxDistance - 4) AnsiColor.YELLOW
				else AnsiColor.GREEN
			}

			// pick a character and color, using ANSI codes;
			def renderTile(c: Char, address: Address): String = {
				distances.get(address) match {
					case Some(distance) =>
						// tile is part of the pipe loop
						val color = pipeColor(distance)
						s"$color${ pipeCharacters(c) }${ AnsiColor.RESET }"
					case None =>
						// tile is part of the ground; we'll distinguish 'inside' vs 'outside' tiles
						if (insideTiles.contains(address)) {
							s"${ AnsiColor.MAGENTA }I${ AnsiColor.RESET }"
						} else {
							"O"
						}
				}
			}

			// loop over the tiles in left->right, top->bottom order,
			// printing each tile separately
			// (trying to create one big string and print it at once
			// seems to cause issues where the ANSI codes sometimes
			// don't render properly in the console)
			for (row, y) <- maze.rows.iterator.zipWithIndex do
				for (c, x) <- row.iterator.zipWithIndex do
					val here = Address(x, y)
					print(renderTile(c, here))
				println()
		}
	}

	// type alias for distances in the pipe loop
	opaque type Distance = Long

	// grid directions where positive y is down
	enum Direction(val dx: Int, val dy: Int):
		case Up extends Direction(0, -1)
		case Down extends Direction(0, 1)
		case Left extends Direction(-1, 0)
		case Right extends Direction(1, 0)

		def opposite: Direction = this match
			case Up => Down
			case Down => Up
			case Left => Right
			case Right => Left

	object Direction:
		val all: Set[Direction] = Set(Up, Down, Left, Right)

	// a location in the MazeInput grid
	case class Address(x: Int, y: Int):
		def +(dir: Direction): Address = Address(x + dir.dx, y + dir.dy)

	// wrapper for the Pipe Loop distance map, to memoize the `maxDistance`
	case class ComputedDistances(distances: Map[Address, Distance]):
		lazy val maxDistance: Distance = distances.values.max
		def get(addr: Address): Option[Distance] = distances.get(addr)
		def visited: Set[Address] = distances.keySet

	// part 1 solver; essentially Dijkstra's algorithm, treating the maze tiles as a graph
	def explorePipeLoop(map: MazeInput): ComputedDistances = {
		val startAddress = map.findStart

		// exploration queue item
		case class ToVisit(address: Address, distance: Distance)

		// allow us to create a PriorityQueue with ToVisit elements
		given Ordering[ToVisit] = Ordering.by(-_.distance)

		// initialize the mutable state of the search
		val pq = mutable.PriorityQueue.empty[ToVisit]
		val distances = mutable.Map.empty[Address, Distance]
		pq.enqueue(ToVisit(startAddress, 0))

		// perform the search:
		// at each step grab the next address with the shortest distance,
		// and if it is shorter than any previously recorded distance,
		// add its neighbors to the exploration queue
		while (pq.nonEmpty) {
			val ToVisit(here, currentDistance) = pq.dequeue
			if distances.get(here).fold(true)(currentDistance < _) then {
				val neighborsToVisit = map.outNeighbors(here).map {
					ToVisit(_, currentDistance + 1)
				}
				pq.enqueue(neighborsToVisit.toSeq: _*)
				distances.update(here, currentDistance)
			}
		}

		ComputedDistances(distances.toMap)
	}

	/*
	Model for "enclosed" check:

	Turns out the Part 1 pipe-loop puzzle is just a maze in disguise.
	The pipes on each input tile form the walls of the maze, and the
	paths in the maze can be represented as the spaces between the tiles.

	I'll establish a convention that the upper-left corner of a tile is
	addressed by the same `Address` as that tile, i.e. corner (x, y) is
	the upper-left corner of tile (x, y).

	Corners can be adjacent to other corners in the four cardinal directions,
	but only if there is a path between them that does not cross any pipes.
	E.g. to reach the corner to the right of a given corner (x, y), we must inspect
	the tiles to the right and below the current corner [(x+1, y-1) and (x+1, y)].
	If the (x+1, y-1) tile has a downward pipe, or the (x+1, y) tile has an upward pipe,
	the pipe is blocking the rightward path from corner (x, y) to corner (x+1, y).

	A tile is considered "enclosed" by the loop if all four of its corners are enclosed
	i.e. [(x, y), (x+1, y), (x, y+1), and (x+1, y+1)].
	A corner is considered "enclosed" if there is no path from that corner to
	the outside of the maze.
	Conversely, a corner is considered "outside" if there exists a path from that corner
	to the outside of the maze.

	My approach is to perform a search starting from the (x=0, y=0) corner of the maze,
	and mark all reachable corners as "outside". Then, iterate over all tile addresses
	of the map; any tile that is part of the pipe can be ignored. Any other tile can
	be checked for its corners' reachability from the outside; if all four corners
	are unreachable, the tile is enclosed and can be counted in the output.
	 */
	class Part2(
		maze: MazeInput,
		pipes: Set[Address], // use the `.keySet` of the `distances` map from part 1
	) {

		// rectangle bounds for limiting search space
		case class Bounds(upperLeft: Address, lowerRight: Address):
			def contains(addr: Address): Boolean = {
				addr.x >= upperLeft.x && addr.x <= lowerRight.x &&
					addr.y >= upperLeft.y && addr.y <= lowerRight.y
			}

		def tileHasOutgoingPipe(here: Address, direction: Direction): Boolean = {
			// Not all tiles are considered part of the pipe loop, so start
			// by checking if `here` is part of that loop.
			// Then, see what directions the pipe leaves `here` from,
			// to see if the requested direction is in that set.
			pipes.contains(here) && maze.outDirections(here).contains(direction)
		}

		// for an edge defined by the upper-left corner of `here`, in the given `direction` for one tile,
		// check if there is a pipe crossing that edge
		def checkBlockedDirectionFromCorner(here: Address, direction: Direction) = direction match {
			case Direction.Up =>
				tileHasOutgoingPipe(here + Direction.Up + Direction.Left, Direction.Right) ||
					tileHasOutgoingPipe(here + Direction.Up, Direction.Left)

			case Direction.Down =>
				tileHasOutgoingPipe(here + Direction.Left, Direction.Right) ||
					tileHasOutgoingPipe(here, Direction.Left)

			case Direction.Left =>
				tileHasOutgoingPipe(here + Direction.Up + Direction.Left, Direction.Down) ||
					tileHasOutgoingPipe(here + Direction.Left, Direction.Up)

			case Direction.Right =>
				tileHasOutgoingPipe(here + Direction.Up, Direction.Down) ||
					tileHasOutgoingPipe(here, Direction.Up)
		}

		// Part 2 solution; find all tiles that are "inside" the pipe loop.
		// (see larger comment above for explanation of the approach)
		def findInsideTiles: Set[Address] = {
			// get the bounds of the pipes, so we can limit the search space
			val pipeXMin = pipes.view.map(_.x).min
			val pipeXMax = pipes.view.map(_.x).max
			val pipeYMin = pipes.view.map(_.y).min
			val pipeYMax = pipes.view.map(_.y).max

			// expand the search area by 1 in each direction,
			// so we know we are starting outside the pipe loop
			val searchBounds = Bounds(
				upperLeft = Address(pipeXMin - 1, pipeYMin - 1),
				lowerRight = Address(pipeXMax + 2, pipeYMax + 2),
			)

			// BFS exploration of reachable corners
			@tailrec
			def explore(queue: List[Address], seen: Set[Address]): Set[Address] = queue match {
				case Nil => seen
				case here :: tail =>
					val neighborsToEnqueue: Iterator[Address] = for {
						dir <- Direction.all.iterator
						if !checkBlockedDirectionFromCorner(here, dir)
						neighbor = here + dir
						if searchBounds.contains(neighbor)
						if !seen.contains(neighbor)
					} yield neighbor

					explore(neighborsToEnqueue.toList ::: tail, seen + here)
			}

			val outsideCorners = explore(searchBounds.upperLeft :: Nil, Set.empty)

			def isOutsideTile(here: Address) = {
				!searchBounds.contains(here) || (
					outsideCorners.contains(here) &&
						outsideCorners.contains(here + Direction.Right) &&
						outsideCorners.contains(here + Direction.Down) &&
						outsideCorners.contains(here + Direction.Right + Direction.Down)
					)
			}

			def isInsideTile(here: Address) = {
				!isOutsideTile(here) && !pipes.contains(here)
			}

			val allTiles: Iterator[Address] = for {
				(row, y) <- maze.rows.iterator.zipWithIndex
				(c, x) <- row.iterator.zipWithIndex
			} yield Address(x, y)

			allTiles.filter(isInsideTile).toSet
		}
	}
}
