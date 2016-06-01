package runtime

import auction._
import card._

object BridgePlayer {
  def main(args: Array[String]): Unit = {
    println("Hello, let's play Bridge!")

    val d = new Deck()
    val (north,east,south,west) = d.deal

    println("The hands are as follows:\n")

    println("North:")
    north.print()
    println("East:")
    east.print()
    println("South:")
    south.print()
    println("West:")
    west.print()

    val a = new Auction(North)
    a.run(Map(North -> YellowCardBidder(north,North), East -> YellowCardBidder(east,East), South -> YellowCardBidder(south,South), West -> YellowCardBidder(west,West)))
  }
}
