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
    import self._

    def all: Iterable[Order] = asks.orders.values.flatten ++ bids.orders.values.flatten
    def best(tpe: OrderType): Option[Order] = side(tpe).best
    def side(tpe: OrderType): Side = tpe.askBid(asks, bids)

    def append(order: Order): OrderBook = order.tpe.askBid(copy(asks = asks.appendOrder(order)), copy(bids = bids.appendOrder(order)))
    def removeBest(tpe: OrderType): OrderBook = tpe.askBid(copy(asks = asks.withoutBest), copy(bids = bids.withoutBest))
    def replaceBestBy(order: Order): OrderBook = removeBest(order.tpe).append(order)

    def clientsPortfolio: ClientsPortfolio = collectClientsPortfolio(all)
    private def collectClientsPortfolio(xs: Iterable[Order]): ClientsPortfolio =
      Monoid[ClientsPortfolio].combineAll(xs.map(_.clientSpend))
  }
}
