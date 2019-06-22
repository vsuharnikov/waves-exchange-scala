package org.github.vsuharnikov.wavesexchange.domain

sealed trait OrderType extends Product with Serializable
object OrderType {
  case object Ask extends OrderType
  case object Bid extends OrderType

  implicit final class Ops(val self: OrderType) extends AnyVal {
    def askBid[T](ifAsk: => T, ifBid: => T): T = if (self == OrderType.Ask) ifAsk else ifBid
    def opposite: OrderType = askBid(Bid, Ask)
  }
}
