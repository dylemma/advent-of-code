package io.dylemma.aoc
package solutions.y2023

import Utils.{ AsInt, Reverse }

import scala.math.Ordering.Implicits.*

object Solution07 extends Solution.NoArgs with Logging {
	def run(input: String): Unit = {

		val parsedInputs = input
			.linesIterator
			.map: line =>
				line.split(' ') match {
					case Array(cards, AsInt(amount)) => Hand(cards) -> Bid(amount)
					case s => throw new RuntimeException(s"Invalid line: '$line'")
				}
			.toList

		val p1Sum = parsedInputs
			.sortBy(_._1)
			.iterator
			.zipWithIndex
			.map { case ((hand, bid), index) =>
				val rank = index + 1
				log.debug(f"[rank $rank%3d] $hand (${ hand.classification }) -> $bid")
				bid.amount * rank.toLong
			}
			.sum

		log.info(s"Total winnings: $p1Sum")

		val p2Sum = parsedInputs
			.map { case (hand, bid) => hand.toImproved -> bid }
			.sortBy(_._1)
			.iterator
			.zipWithIndex
			.map { case ((hand, bid), index) =>
				val rank = index + 1
				if (hand.original.cards.contains('J')) {
					log.debug(f"[rank $rank%3d] ${ hand.original } ~> ${ hand.improved } (${ hand.improved.classification }) -> $bid")
				}
				bid.amount * rank.toLong
			}
			.sum
		log.info(s"Total winnings with jokers: $p2Sum")
	}

	val p1RankOf: Char => Int = "23456789TJQKA".iterator.zipWithIndex.toMap
	val p2RankOf: Char => Int = "J23456789TQKA".iterator.zipWithIndex.toMap

	case class Bid(amount: Long)

	case class Hand(cards: String) {
		lazy val classification: Classification = {
			val rankCounts = cards.groupBy(identity).view.mapValues(_.length).toList
			val counts = rankCounts.map(_._2).sorted.reverse

			counts match {
				case List(5) => Classification.FiveOfAKind
				case List(4, 1) => Classification.FourOfAKind
				case List(3, 2) => Classification.FullHouse
				case List(3, 1, 1) => Classification.ThreeOfAKind
				case List(2, 2, 1) => Classification.TwoPair
				case List(2, 1, 1, 1) => Classification.OnePair
				case List(1, 1, 1, 1, 1) => Classification.HighCard
				case _ => throw new RuntimeException(s"Invalid hand: $cards")
			}
		}

		lazy val cardRanks: LazyList[Int] = cards.iterator.map(p1RankOf).to(LazyList)

		def toImproved = {
			lazy val improved: Hand = {
				if (cards.contains('J')) {
					val candidateLetters = cards.iterator.filter(_ != 'J').toSet
					candidateLetters.iterator.map { replacement =>
						Hand(cards.replace('J', replacement))
					}.maxOption.getOrElse(this)
				} else {
					this
				}
			}
			ImprovedHand(original = this, improved)
		}
	}

	object Hand:
		given Ordering[Hand] = (x: Hand, y: Hand) => {
			summon[Ordering[Classification]].compare(x.classification, y.classification) match {
				case 0 => x.cardRanks.zip(y.cardRanks).map { _ - _ }.find(_ != 0).getOrElse(0)
				case other => other
			}
		}

	enum Classification(val rank: Int):
		case HighCard extends Classification(1)
		case OnePair extends Classification(2)
		case TwoPair extends Classification(3)
		case ThreeOfAKind extends Classification(4)
		case FullHouse extends Classification(5)
		case FourOfAKind extends Classification(6)
		case FiveOfAKind extends Classification(7)

	object Classification:
		given Ordering[Classification] = Ordering.by(_.rank)

	case class ImprovedHand(original: Hand, improved: Hand) {
		lazy val cardRanks: LazyList[Int] = original.cards.iterator.map(p2RankOf).to(LazyList)
	}

	object ImprovedHand:
		given Ordering[ImprovedHand] = (x: ImprovedHand, y: ImprovedHand) => {
			summon[Ordering[Classification]].compare(x.improved.classification, y.improved.classification) match {
				case 0 => x.cardRanks.zip(y.cardRanks).map { _ - _ }.find(_ != 0).getOrElse(0)
				case other => other
			}
		}
}
