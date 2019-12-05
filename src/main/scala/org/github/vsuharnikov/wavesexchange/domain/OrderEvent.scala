package org.github.vsuharnikov.wavesexchange.domain

sealed trait OrderEvent
object OrderEvent {
  case class Added(order: LimitOrder) extends OrderEvent
  case class Executed(maker: LimitOrder, taker: LimitOrder) extends OrderEvent
  case class Canceled(order: LimitOrder) extends OrderEvent
}
