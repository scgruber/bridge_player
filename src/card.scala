package card

import scala.util.Random

class Deck {
  val d : Set[Card] = Array(Spade,Heart,Diamond,Club).flatMap(s =>
    Array(Card(s,Ace), Card(s,King), Card(s,Queen), Card(s,Jack),
      Card(s,Ten), Card(s,Nine), Card(s,Eight), Card(s,Seven), Card(s,Six),
      Card(s,Five), Card(s,Four), Card(s,Three), Card(s,Two))).toSet
  def deal: (Hand,Hand,Hand,Hand) =  {
    val dRand = Random.shuffle(d.toList).toArray
    val north = new Hand(dRand.slice(0,13))
    val east = new Hand(dRand.slice(13,26))
    val south = new Hand(dRand.slice(26,39))
    val west = new Hand(dRand.slice(39,52))
    (north,east,south,west)
  }
}

class Hand(cards: Array[Card]) {
  var cs = cards.map(c => Some(c))

  def points: Int = {
    cs collect {
      case Some(Card(_,Ace)) => 4
      case Some(Card(_,King)) => 3
      case Some(Card(s,Queen)) => cs.count {
        case Some(Card(`s`,_)) => true
        case _ => false
      } match {
        case 1 => 1
        case _ => 2
      }
      case Some(Card(_,Jack)) => 1
    } sum
  }

  def pointsInSuit(s: Suit): Int = {
    cs collect {
      case Some(Card(`s`,Ace)) => 4
      case Some(Card(`s`,King)) => 3
      case Some(Card(`s`,Queen)) => cs.count {
        case Some(Card(`s`,_)) => true
        case _ => false
      } match {
        case 1 => 1
        case _ => 2
      }
      case Some(Card(`s`,Jack)) => 1
    } sum
  }

  def print(): Unit = {
    for (s: Suit <- Array(Spade, Heart, Diamond, Club)) {
      val inSuit = cs collect {
        case Some(Card(`s`, r)) => Card(s, r)
      }
      inSuit.sortBy(c => c.rank.magnitude).toList match {
        case List() => Unit
        case inSuitCards => println(s.toString + ": " + inSuitCards.map(c => c.rank.toStringShort).reduce((a: String, b: String) => a.concat(", ").concat(b)))
      }
    }
    println("Points: " + this.points.toString)
    println
  }

  def isBalanced: Boolean = {
    this.distribution.forall { case (_,ct) => (2 <= ct) && (ct <= 4) }
  }

  def distribution: Map[Suit, Int] = {
    Map(Spade -> cs.count { case Some(Card(Spade,_)) => true; case _ => false },
      Heart -> cs.count { case Some(Card(Heart,_)) => true; case _ => false },
      Diamond -> cs.count { case Some(Card(Diamond,_)) => true; case _ => false },
      Club -> cs.count { case Some(Card(Club,_)) => true; case _ => false })
  }
}

case class Card(s:Suit,r:Rank) {
  def suit: Suit = this.s
  def rank: Rank = this.r
  override def toString: String = {
    this.rank.toString + " of " + this.suit.toString + "s"
  }
}

abstract class Suit extends Ordered[Suit] {
  def toString: String
  val magnitude: Int

  def compare(that: Suit): Int = this.magnitude - that.magnitude
}
abstract class NoSuit extends Suit
abstract class MajorSuit extends Suit
abstract class MinorSuit extends Suit
case object NoTrump extends NoSuit {
  override def toString: String = "No Trump"
  val magnitude = 5
}
case object Spade extends MajorSuit {
  val magnitude = 4
}
case object Heart extends MajorSuit {
  val magnitude = 3
}
case object Diamond extends MinorSuit {
  val magnitude = 2
}
case object Club extends MinorSuit {
  val magnitude = 1
}

abstract class Rank extends Ordered[Rank] {
  def toString: String
  def toStringShort: String
  val magnitude: Int
  val points: Int

  def compare(that: Rank): Int = this.magnitude - that.magnitude
}
case object Ace extends Rank {
  override def toString: String = "Ace"
  def toStringShort = "A"
  val magnitude = 14
  val points = 4
}
case object King extends Rank {
  override def toString: String = "King"
  def toStringShort = "K"
  val magnitude = 13
  val points = 3
}
case object Queen extends Rank {
  override def toString: String = "Queen"
  def toStringShort = "Q"
  val magnitude = 12
  val points = 2
}
case object Jack extends Rank {
  override def toString: String = "Jack"
  def toStringShort = "J"
  val magnitude = 11
  val points = 1
}
case object Ten extends Rank {
  override def toString: String = "Ten"
  def toStringShort = "10"
  val magnitude = 10
  val points = 0
}
case object Nine extends Rank {
  override def toString: String = "Nine"
  def toStringShort = "9"
  val magnitude = 9
  val points = 0
}
case object Eight extends Rank {
  override def toString: String = "Eight"
  def toStringShort = "8"
  val magnitude = 8
  val points = 0
}
case object Seven extends Rank {
  override def toString: String = "Seven"
  def toStringShort = "7"
  val magnitude = 7
  val points = 0
}
case object Six extends Rank {
  override def toString: String = "Six"
  def toStringShort = "6"
  val magnitude = 6
  val points = 0
}
case object Five extends Rank {
  override def toString: String = "Five"
  def toStringShort = "5"
  val magnitude = 5
  val points = 0
}
case object Four extends Rank {
  override def toString: String = "Four"
  def toStringShort = "4"
  val magnitude = 4
  val points = 0
}
case object Three extends Rank {
  override def toString: String = "Three"
  def toStringShort = "3"
  val magnitude = 3
  val points = 0
}
case object Two extends Rank {
  override def toString: String = "Two"
  def toStringShort = "2"
  val magnitude = 2
  val points = 0
}