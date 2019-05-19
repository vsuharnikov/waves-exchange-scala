package org.github.vsuharnikov.wavesexchange.domain

import cats.implicits._
import cats.kernel.Monoid
import io.estatico.newtype.macros.newtype
import org.github.vsuharnikov.wavesexchange.domain.OrderBook.{Asks, Bids}

import scala.collection.immutable.SortedSet

case class OrderBook private (asks: Asks, bids: Bids)

object OrderBook {
  type Side = SortedSet[Order]

  @newtype case class Asks(side: Side)
  object Asks {
    val order: Ordering[Order] = Ordering.by[Order, Int](_.pricePerOne)
  }

  @newtype case class Bids(side: Side)
  object Bids {
    val order: Ordering[Order] = Asks.order.reverse
  }

  val empty = new OrderBook(Asks(SortedSet.empty(Asks.order)), Bids(SortedSet.empty(Bids.order)))

  def apply(asks: List[Order], bids: List[Order]): OrderBook = empty.copy(
    asks = Asks(empty.asks.side ++ asks),
    bids = Bids(empty.bids.side ++ bids)
  )

  final implicit class Ops(val self: OrderBook) extends AnyVal {
    def all: List[Order] = self.asks.side.toList ++ self.bids.side.toList
    def best(tpe: OrderType): Option[Order] = self.side(tpe).headOption
    def side(tpe: OrderType): Side = if (tpe == OrderType.Bid) self.bids.side else self.asks.side

    def append(order: Order): OrderBook =
      if (order.tpe == OrderType.Ask) self.copy(asks = Asks(self.asks.side + order))
      else self.copy(bids = Bids(self.bids.side + order))

    def removeBest(tpe: OrderType): OrderBook =
      if (tpe == OrderType.Ask) self.copy(asks = Asks(self.asks.side.tail))
      else self.copy(bids = Bids(self.bids.side.tail))

    def replaceBestBy(order: Order): OrderBook =
      if (order.tpe == OrderType.Ask) self.copy(asks = Asks(self.asks.side.tail + order))
      else self.copy(bids = Bids(self.bids.side.tail + order))

    def clientsPortfolio: ClientsPortfolio = collectClientsPortfolio(self.all)

    private def collectClientsPortfolio(xs: List[Order]): ClientsPortfolio =
      Monoid[ClientsPortfolio].combineAll(xs.map(_.clientSpend))
  }
}
