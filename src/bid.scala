package bid

import auction._
import card._

abstract class Call {
  def legal(auction: Auction): Boolean
}
case object Pass extends Call {
  def legal(auction: Auction) = true
}
case class Double(bid: Bid) extends Call {
  def legal(auction: Auction) = auction.bids match {
    case `bid`::_ => true
    case Pass::Pass::`bid`::_ => true
    case _ => false
  }
}
case class Redouble(bid: Bid) extends Call {
  def legal(auction: Auction) = auction.bids match {
    case Double(`bid`)::_ => true
    case Pass::Pass::Double(`bid`)::_ => true
    case _ => false
  }
}
case class Bid(s: Suit, m: Int) extends Call {
  def legal(auction: Auction) = (m > 0) && (m < 8) match {
    case true => auction.bids.dropWhile { case Pass|Double(_)|Redouble(_) => true; case _ => false } match {
      case Bid(t, n)::_ => (m compare n).signum match {
        case 1 => true
        case 0 => ((s.magnitude) compare (t.magnitude)).signum match {
          case 1 => true
          case _ => false
        }
        case -1 => false
      }
      case List() => true
    }
    case false => false
  }
}

abstract class BidResult
case object Hung extends BidResult
case object Done extends BidResult
case object Continue extends BidResult
case class Illegal(info: String) extends BidResult