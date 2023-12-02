package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Inputs

import cats.effect.SyncIO

object Puzzle2 {
	sealed abstract class RPS(val value: Int, val beats: RPS => Boolean)
	object RPS {
		case object Rock extends RPS(1, _ == Scissors)
		case object Paper extends RPS(2, _ == Rock)
		case object Scissors extends RPS(3, _ == Paper)
		val all = Rock :: Paper :: Scissors :: Nil

		def unapply(letter: String) = letter match {
			case "A" | "X" => Some(Rock)
			case "B" | "Y" => Some(Paper)
			case "C" | "Z" => Some(Scissors)
			case _ => None
		}

		def choose(opp: RPS, desiredOutcome: Outcome) = desiredOutcome match {
			case Outcome.Draw => opp
			case Outcome.Win => all.find(_ beats opp).get
			case Outcome.Lose => all.find(opp beats _).get
		}
	}

	sealed abstract class Outcome(val score: Int)
	object Outcome {
		case object Win extends Outcome(6)
		case object Lose extends Outcome(0)
		case object Draw extends Outcome(3)

		def unapply(letter: String) = letter match {
			case "X" => Some(Lose)
			case "Y" => Some(Draw)
			case "Z" => Some(Win)
			case _ => None
		}
	}

	case class Round(opponent: RPS, you: RPS) {
		lazy val score = {
			val outcomeScore =
				if (you beats opponent) 6
				else if (opponent beats you) 0
				else 3
			outcomeScore + you.value
		}
	}
	object Round {
		def fromGuide(guideString: String) = guideString.split(" ") match {
			case Array(RPS(opp), RPS(you)) => Round(opp, you)
			case _ => throw new IllegalArgumentException(s"Bad guide string: $guideString")
		}
		def fromGuide2(guideString: String) = guideString.split(" ") match {
			case Array(RPS(opp), Outcome(out)) => Round(opp, RPS.choose(opp, out))
			case _ => throw new IllegalArgumentException(s"Bad guide string: $guideString")
		}
	}

	def main(args: Array[String]): Unit = {
		val example = "A Y\nB X\nC Z"
		val exampleSum = example.linesIterator.map(Round.fromGuide).map(_.score).sum
		println(s"example sum: $exampleSum")

		val totalScore = Inputs
			.lineStream[SyncIO]("/input_2022_2.txt")
			.filter(_.nonEmpty)
			.map(Round.fromGuide)
			// .evalTap(r => SyncIO { println(s"$r - score = ${r.score}")})
			.map(_.score)
			.compile
			.fold(0)(_ + _)
			.unsafeRunSync()
		println(s"Total score: $totalScore")

		// Second half:
		val totalScore2 = Inputs
			.lineStream[SyncIO]("/input_2022_2.txt")
			.filter(_.nonEmpty)
			.map(Round.fromGuide2)
			.map(_.score)
			.compile
			.fold(0)(_ + _)
			.unsafeRunSync()
		println(s"Total V2 score: $totalScore2")
	}
}
