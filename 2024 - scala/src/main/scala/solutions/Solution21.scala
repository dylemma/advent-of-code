package io.dylemma.aoc
package solutions

import Utils.{ AsTuple2, sumTuples }

object Solution21 extends Solution.NoArgs with Logging {
	override def run(input: String): Unit = {
		val numpad: Keyboard = new Keyboard(
			"""789
			  |456
			  |123
			  | 0A""".stripMargin,
		)

		val dirpad = new Keyboard(
			""" ^A
			  |<v>""".stripMargin,
		)

		log.debug(s"Numpad: $numpad")
		log.debug(s"Dirpad: $dirpad")

		val navCache = new NavigationCache(numpad, dirpad)

		def complexity(input: String): (Int, Long) = {
			val numericPart = input.filter(_.isDigit).toInt

			// Compute Part 1 values (and recompute using p2 logic)
			val expandedInput = navCache.expandPath(input, 2)
			val p1Cost = expandedInput.length
			val p1Product = numericPart * p1Cost

			// Compute Part 2 values
			val p2Cost = navCache.getExpandedSequenceCost(input, 25)
			val p2Product = numericPart * p2Cost

			// Introspection
			log.info(s"Input $input: $expandedInput")

			log.debug(s"  expand^2 = $expandedInput")
			log.debug(s"  expand^2 cost = $p1Cost")
			log.debug(s"  expand^25 cost = $p2Cost")
			log.info(s"  Part 1 Complexity: $numericPart * $p1Cost = $p1Product")
			log.info(s"  Part 2 Complexity: $numericPart * $p2Cost = $p2Product")

			p1Product -> p2Product
		}

		val (p1Sum, p2Sum) = input.linesIterator.map(complexity).sumTuples
		log.info(s"Part 1 Result: $p1Sum")
		log.info(s"Part 2 Result: $p2Sum")
	}

	enum Direction(val key: Char, val delta: Delta):
		case Up extends Direction('^', Delta(0, 1))
		case Down extends Direction('v', Delta(0, -1))
		case Left extends Direction('<', Delta(-1, 0))
		case Right extends Direction('>', Delta(1, 0))

	case class Pos(x: Int, y: Int):
		def -(that: Pos): Delta = Delta(this.x - that.x, this.y - that.y)
		def +(delta: Delta): Pos = Pos(this.x + delta.dx, this.y + delta.dy)

	case class Delta(dx: Int, dy: Int) {
		def horizontalOnly = Delta(dx, 0)
		def verticalOnly = Delta(0, dy)

		def hSteps: Iterator[Direction] =
			if (dx < 0) Iterator.fill(-dx)(Direction.Left)
			else Iterator.fill(dx)(Direction.Right)
		def vSteps: Iterator[Direction] =
			if (dy < 0) Iterator.fill(-dy)(Direction.Down)
			else Iterator.fill(dy)(Direction.Up)
	}

	class Keyboard(layout: String) {
		override def toString = s"Keyboard(${ layout.linesIterator.mkString("|") })"

		private val charPositions: Map[Char, Pos] = {
			// last row will be y=0
			// first row will be y=(rows.size - 1)
			val rows = layout.linesIterator.toVector
			(for {
				(row, rowIndex) <- rows.zipWithIndex
				y = rows.size - 1 - rowIndex
				(char, x) <- row.zipWithIndex
				if char != ' '
			} yield {
				char -> Pos(x, y)
			}).toMap
		}
		private val positions: Map[Pos, Char] = charPositions.map(_.swap)

		def keys: Iterable[Char] = charPositions.keys
		def positionOf(char: Char): Pos = charPositions.getOrElse(char, {
			throw new NoSuchElementException(s"No '$char' on this keyboard")
		})
		def charAt(pos: Pos): Option[Char] = positions.get(pos)
	}

	/** Find all(-ish) paths from the start to goal position on the given keyboard.
	  *
	  * A minor optimization here is that we will omit "zigzag" paths, since intuitively
	  * they will cause more key-presses when the resulting path is expanded.
	  * (It only takes an 'A' to press a key the robot is already pointing at, so
	  * consecutive presses of the same button save key-presses.)
	  *
	  * @param start    Current position of a robot
	  * @param goal     Goal position of a robot
	  * @param keyboard The keyboard the robot is navigating
	  * @return A series of possible paths that the robot can take to get from start to goal
	  */
	def findDirectionsCandidates(start: Pos, goal: Pos, keyboard: Keyboard): List[Vector[Direction]] = {
		val delta = goal - start
		val candidates = List(
			(delta.hSteps ++ delta.vSteps).toVector,
			(delta.vSteps ++ delta.hSteps).toVector,
		)
		candidates.filter { path =>
			path.iterator
				.scanLeft(start) { _ + _.delta }
				.forall { keyboard.charAt(_).isDefined }
		}.distinct
	}

	def findBestDirections(start: Pos, goal: Pos, keyboard: Keyboard, navigationCache: Map[(Char, Char), String]): String = {
		findDirectionsCandidates(start, goal, keyboard).minBy { path =>
			val sequence = path.map(_.key).mkString
			//
			path.sliding(2).map {
				case AsTuple2(d1, d2) if d1 != d2 =>
					navigationCache.get(d1.key -> d2.key).fold(0)(_.length)
				case _ => 0
			}.sum

		}
		val delta @ Delta(dx, dy) = goal - start

		val startChar = keyboard.charAt(start).getOrElse {
			throw new IllegalArgumentException(s"Start position $start is not on the keyboard")
		}
		val goalChar = keyboard.charAt(goal).getOrElse {
			throw new IllegalArgumentException(s"Goal position $goal is not on the keyboard")
		}
		navigationCache.getOrElse((startChar, goalChar), {
			throw new IllegalArgumentException(s"No cached navigation from '$startChar' to '$goalChar'")
		})
	}

	/** Given a robot (1) currently pointing at 'A' on its keyboard, and a second robot (2)
	  * whose keyboard controls robot 1, find sequences of key-presses that robot 2 can use
	  * to make robot 1 type out the given sequence of characters.
	  *
	  * For example, the sequence "v" can be produced by robot 2 typing either `<vA` or `v<A`.
	  * The sequence `vv` can be produced by robot 2 typing either: `<vAA` or `v<AA`.
	  * The sequence `vA` can be produced by robot 2 typing `<vA>^A` or several other paths.
	  *
	  * @param sequence A string of characters that Robot 1 should type on its keyboard
	  * @param keyboard The keyboard used by Robot 1
	  * @return A list of possible sequences that can be typed by Robot 1 in order to
	  *         cause Robot 2 to type the given `sequence`
	  */
	def expandPaths(sequence: String, keyboard: Keyboard): List[String] = {
		val out = List.newBuilder[String]

		def dfs(head: Char, tail: List[Char], keyboard: Keyboard, accumPath: List[Char]): Unit = tail match {
			case next :: tail2 =>
				val pos1 = keyboard.positionOf(head)
				val pos2 = keyboard.positionOf(next)
				for (path <- findDirectionsCandidates(pos1, pos2, keyboard)) {
					val segment = path.map(_.key) :+ 'A'
					dfs(next, tail2, keyboard, accumPath ++ segment)
				}
			case Nil =>
				out += accumPath.mkString
		}

		dfs('A', sequence.toList, keyboard, Nil)
		out.result()
	}

	/** For each pair of keys on the given keyboard, compute the best sequence (e.g. `>>^A`)
	  * to get from one key to the other and press that other key.
	  *
	  * This is the part of the puzzle I was most frustrated with. I saw a few other people's
	  * solutions which hard-coded these best paths for the arrow keys. This programmatic
	  * solution should in theory have worked fine, but it initially missed the surprise
	  * outcome that `>^A` is a worse path than `^>A`. The insight I had was that by
	  * double-expanding the candidates, one candidate would be shorter than the other,
	  * but in this case the result was the same length for both candidates. I assumed
	  * that would mean it wouldn't matter which one was picked, but it did end up mattering.
	  *
	  * A tie-breaker was added in the underlying logic, specifically to force the choice
	  * of `^>A` instead of `>^A` for moving the cursor from "v" to "A".
	  *
	  * @param keyboard The keyboard whose key combinations will be computed
	  * @param dirpad   The directional keyboard
	  * @return A map of each `(key1, key2)` combination, with the best expanded route
	  *         between the two keys (including the `A` suffix to press the target key)
	  */
	def precomputeBestRoutes(keyboard: Keyboard, dirpad: Keyboard): Map[(Char, Char), String] = {
		(for {
			key1 <- keyboard.keys.view
			key2 <- keyboard.keys.view
			paths = findDirectionsCandidates(keyboard.positionOf(key1), keyboard.positionOf(key2), keyboard)
				.map { _.map(_.key).mkString }
		} yield pickBestRoute(paths, key1, key2, dirpad)).toMap
	}

	def pickBestRoute(paths: List[String], key1: Char, key2: Char, dirpad: Keyboard): ((Char, Char), String) = {
		log.debug(s"Computing best route from '$key1' to '$key2'")
		paths match {
			case Nil =>
				throw new IllegalArgumentException(s"No paths from '$key1' to '$key2' in $dirpad")
			case one :: Nil =>
				log.debug(s"  Only one path: [$one]")
				(key1, key2) -> (one :+ 'A')
			case multiple =>
				// There are multiple possible paths; find the best one by measuring
				// minimum the length of its twice-expanded forms.
				// (Expanding once always results in the same sequence length,
				// but depending on the specific sequence, expanding again can
				// yield different lengths.)
				val bestPath = paths.map(_ :+ 'A').minBy { candidate =>
					val shortestExpansion = expandPaths(candidate, dirpad)
						.flatMap { expandPaths(_, dirpad) }
						.flatMap { expandPaths(_, dirpad) }
						.minBy(_.length)
					log.debug(s"  Candidate path [$candidate] expands to length ${ shortestExpansion.length } [$shortestExpansion]")
					// tie-breaker for length: prefer vertical moves first
					//   ...not really sure WHY but without this the solution is wrong
					shortestExpansion.length -> !(candidate.startsWith("^") || candidate.startsWith("v"))
				}
				log.debug(s"  Chose best path: [$bestPath]")
				(key1, key2) -> bestPath
		}
	}

	case class Step(from: Char, to: Char)

	case class KeyboardWithPaths(name: String, keyboard: Keyboard, bestPaths: Map[(Char, Char), String]) {
		def getPath(step: Step): String = getPath(step.from, step.to)
		def getPath(from: Char, to: Char): String = {
			bestPaths.getOrElse(from -> to, {
				throw new IllegalArgumentException(s"No best path from '$from' to '$to' in $keyboard")
			})
		}

		if (log.isDebugEnabled) {
			for {
				c1 <- keyboard.keys.toList.sorted
				c2 <- keyboard.keys.toList.sorted
			} {
				val route = bestPaths(c1 -> c2)
				log.debug(s"$name Route from '$c1' to '$c2': [$route]")
			}
		}
	}

	class NavigationCache(_numpad: Keyboard, _dirpad: Keyboard) {

		val dirpad = KeyboardWithPaths("Dirpad", _dirpad, precomputeBestRoutes(_dirpad, _dirpad))
		val numpad = KeyboardWithPaths("Numpad", _numpad, precomputeBestRoutes(_numpad, _dirpad))

		private def keyboardAt(level: Int): KeyboardWithPaths = if level == 0 then numpad else dirpad

		private def steps(sequence: String): Iterator[Step] = {
			('A' +: sequence).sliding(2).map { group =>
				Step(group.charAt(0), group.charAt(1))
			}
		}

		def expandPath(sequence: String, numeric: Boolean) = {
			val keyboard = if numeric then numpad else dirpad
			steps(sequence).map { keyboard.getPath(_) :+ 'A' }.mkString
		}

		// part 1 version that evaluates the real string path
		def expandPath(sequence: String, n: Int): String = {
			require(n >= 0, s"n must be non-negative, got $n")

			def getExpandedStep(step: Step, level: Int): String = {
				val stepPath = keyboardAt(level).getPath(step)
				if (level == n) stepPath else getExpandedSequence(stepPath, level + 1)
			}

			def getExpandedSequence(sequence: String, level: Int): String = {
				steps(sequence).map { getExpandedStep(_, level) }.mkString
			}

			getExpandedSequence(sequence, 0)
		}

		// part 2 version which only gets the cost, since the length becomes so large
		// that the time spent expanding the actual string is prohibitively slow
		def getExpandedSequenceCost(sequence: String, n: Int): Long = {
			val expansionCostCache = collection.mutable.Map.empty[(Step, Int), Long]

			def getExpandedStepCost(step: Step, level: Int): Long = {
				expansionCostCache.getOrElseUpdate(step -> level, {
					val stepPath = keyboardAt(level).getPath(step)
					if (level == n) stepPath.length else getExpandedSequenceCost(stepPath, level + 1)
				})
			}

			def getExpandedSequenceCost(sequence: String, level: Int): Long = {
				steps(sequence).map { getExpandedStepCost(_, level) }.sum
			}

			getExpandedSequenceCost(sequence, 0)
		}
	}

}
