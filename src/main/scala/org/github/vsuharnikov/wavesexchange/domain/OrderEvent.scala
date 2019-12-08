package org.github.vsuharnikov.wavesexchange.domain

sealed trait OrderEvent
object OrderEvent {
  case class New(order: LimitOrder) extends OrderEvent
  case class Placed(order: LimitOrder) extends OrderEvent
  case class Executed(maker: LimitOrder, taker: LimitOrder) extends OrderEvent
}
