package auction

import bid._
import card._

import scala.io.StdIn

class Auction(startPosition: Position) {
  var bids: List[Call] = List()
  var currentPosition: Position = startPosition
  var contract: (Position, Call) = (North, Pass)
  def makeBid(bid: Call): BidResult = {
    bid.legal(this) match {
      case true => {
        bids ::= bid
        bid match {
          case Pass => Unit
          case Double(b) => contract = (contract._1, Double(b))
          case Redouble(b) => contract = (contract._1, Redouble(b))
          case Bid(s,m) => contract = (currentPosition, Bid(s,m))
        }
        currentPosition = currentPosition.next
        this.bids match {
          case Pass::Pass::Pass::Pass::List() => Hung
          case Pass::Pass::Pass::(Double(_)|Redouble(_)|Bid(_,_))::_ => Done
          case _ => Continue
        }
      }
      case false => Illegal(bid.toString)
    }
  }

  def run(bidders: Map[Position,Bidder]): (Position, Call) = {
    var bidResult: BidResult = Continue
    while (bidResult==Continue) {
      bidResult = makeBid(bidders.get(currentPosition).get.makeBid(bids))
    }
    bidResult match {
      case Illegal(info) => throw new Exception("Illegal bid: " + info)
      case Done => {
        println("The contract is " + this.contract._2.toString + " declared by " + this.contract._1.toString)
        this.contract
      }
      case Hung => {
        println("All four players passed.")
        this.contract
      }
    }
  }
}

abstract class Bidder {
  def makeBid(bids: List[Call]): Call
}
case class HumanBidder(h: Hand, p: Position) extends Bidder {
  def makeBid(bids: List[Call]): Call = {
    val bidString = StdIn.readLine(p.toString + ", enter bid:")
    bidString match {
      case "Pass" => Pass
      case "Double" => bids match {
        case Bid(s, m)::_ => Double(Bid(s, m))
        case Pass::Pass::Bid(s,m)::_ => Double(Bid(s,m))
        case _ => throw new Exception("Double over " + bids.take(4).toString)
      }
      case "Redouble" => bids match {
        case Double(Bid(s,m))::_ => Redouble(Bid(s,m))
        case Pass::Pass::Double(Bid(s,m))::_ => Redouble(Bid(s,m))
        case _ => throw new Exception("Redouble over " + bids.take(4).toString)
      }
      case bid => bid.split(" ",2) match {
        case Array(m, "No Trump") => Bid(NoTrump, m.toInt)
        case Array(m, "Spade") => Bid(Spade, m.toInt)
        case Array(m, "Heart") => Bid(Heart, m.toInt)
        case Array(m, "Diamond") => Bid(Diamond, m.toInt)
        case Array(m, "Club") => Bid(Club, m.toInt)
        case _ => throw new Exception("Bad input '" + bid + "'")
      }
    }
  }
}
case class YellowCardBidder(h: Hand, p: Position) extends Bidder {
  def makeBid(bids: List[Call]): Call = {
    val bid = if (isOpening(bids)) {
      if (h.isBalanced) {
        h.points match {
          case x if 15 <= x && x <= 17 => Bid(NoTrump,1)
          case x if 20 <= x && x <= 21 => Bid(NoTrump,2)
          case x if 25 <= x && x <= 27 => Bid(NoTrump,3)
          case _ => Pass
        }
      } else if (h.points >= 22) {
        Bid(Club,2)
      } else if (!hasOpeningStrength(h, bids.size+1) && h.points >= 5) {
        val dist = h.distribution
        if (dist(Spade) >= 6 && suitIsQuality(Spade, h)) {
          Bid(Spade,2)
        } else if (dist(Heart) >= 6 && suitIsQuality(Heart, h)) {
          Bid(Heart,2)
        } else if (dist(Diamond) >= 6 && suitIsQuality(Diamond, h)) {
          Bid(Diamond,2)
        } else {
          Pass
        }
      } else if (hasOpeningStrength(h, bids.size+1)) {
        val dist = h.distribution
        if (dist(Spade) >= 5 || dist(Heart) >= 5) {
          // Prefer the longer (then stronger) major suit
          (dist(Spade) compare dist(Heart)).signum match {
            case 1 => Bid(Spade,1)
            case -1 => Bid(Heart,1)
            case 0 => (h.pointsInSuit(Spade) compare h.pointsInSuit(Heart)).signum match {
              case -1 => Bid(Heart,1)
              case _ => Bid(Spade,1)
            }
          }
        } else if (dist(Diamond) >= 4) {
          Bid(Diamond,1)
        } else if (dist(Club) >= 3) {
          Bid(Club,1)
        } else {
          Pass
        }
      } else {
        Pass
      }
    } else {
      Pass
    }

    println(p.toString + " bids " + bid.toString)
    bid
  }

  private def isOpening(bids: List[Call]): Boolean = {
    bids.forall(_ == Pass)
  }

  private def hasOpeningStrength(h: Hand, turn: Int): Boolean = turn match {
    case (1|2) => h.points >= 13
    case 3 => h.points >= 12
    case 4 => h.points >= 11
  }

  private def suitIsQuality(s:Suit, h:Hand): Boolean = {
    val topThree = h.cs.count { case Some(Card(`s`,(Ace|King|Queen))) => true; case _ => false }
    val topFive = h.cs.count { case Some(Card(`s`,(Ace|King|Queen|Jack|Ten))) => true; case _ => false }
    topThree >= 2 || topFive >= 3
  }
}

abstract class Position {
  def next: Position
}
case object North extends Position {
  def next = East
}
case object East extends Position {
  def next = South
}
case object South extends Position {
  def next = West
}
case object West extends Position {
  def next = North
}