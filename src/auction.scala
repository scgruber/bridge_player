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
case class AIBidder(h: Hand, p: Position) extends Bidder {
  def makeBid(bids: List[Call]): Call = {
    val bid = if (isOpening(bids)) {
      if (h.isBalanced) {
        h.points match {
          case x if 15 <= x && x <= 17 => Bid(NoTrump,1)
          case x if 20 <= x && x <= 21 => Bid(NoTrump,2)
          case x if 25 <= x && x <= 27 => Bid(NoTrump,3)
          case _ => Pass
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