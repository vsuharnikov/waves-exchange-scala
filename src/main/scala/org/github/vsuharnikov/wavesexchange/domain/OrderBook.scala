package org.github.vsuharnikov.wavesexchange.domain

import cats.kernel.Monoid

case class OrderBook private (asks: Side, bids: Side)

object OrderBook {
  val empty = new OrderBook(Side.empty(OrderType.Ask), Side.empty(OrderType.Bid))

  def apply(asks: List[Order], bids: List[Order]): OrderBook = empty.copy(
    asks = empty.asks.appendOrders(asks),
    bids = empty.bids.appendOrders(bids)
  )

  final implicit class Ops(val self: OrderBook) extends AnyVal {
    def all: Iterable[Order] = self.asks.orders.values.flatten ++ self.bids.orders.values.flatten
    def best(tpe: OrderType): Option[Order] = self.side(tpe).best
    def side(tpe: OrderType): Side = if (tpe == OrderType.Bid) self.bids else self.asks

    def append(order: Order): OrderBook =
      if (order.tpe == OrderType.Ask) self.copy(asks = self.asks.appendOrder(order))
      else self.copy(bids = self.bids.appendOrder(order))

    def removeBest(tpe: OrderType): OrderBook =
      if (tpe == OrderType.Ask) self.copy(asks = self.asks.withoutBest)
      else self.copy(bids = self.bids.withoutBest)

    def replaceBestBy(order: Order): OrderBook = self.removeBest(order.tpe).append(order)

    def clientsPortfolio: ClientsPortfolio = collectClientsPortfolio(self.all)

    private def collectClientsPortfolio(xs: Iterable[Order]): ClientsPortfolio =
      Monoid[ClientsPortfolio].combineAll(xs.map(_.clientSpend))
  }
}
