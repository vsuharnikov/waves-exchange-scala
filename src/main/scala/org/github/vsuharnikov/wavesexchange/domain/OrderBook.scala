package org.github.vsuharnikov.wavesexchange.domain

import cats.kernel.Monoid

case class OrderBook private (asks: Side, bids: Side)

object OrderBook {
  val empty = new OrderBook(Side.empty(OrderType.Ask), Side.empty(OrderType.Bid))

  def apply(asks: List[LimitOrder], bids: List[LimitOrder]): OrderBook = empty.copy(
    asks = empty.asks.appendOrders(asks),
    bids = empty.bids.appendOrders(bids)
  )

  final implicit class Ops(val self: OrderBook) extends AnyVal {
    import self._

    def all: Iterable[LimitOrder] = asks.orders.values.flatten ++ bids.orders.values.flatten
    def best(tpe: OrderType): Option[LimitOrder] = side(tpe).best
    def side(tpe: OrderType): Side = tpe.askBid(asks, bids)

    def append(lo: LimitOrder): OrderBook = lo.order.tpe.askBid(copy(asks = asks.appendOrder(lo)), copy(bids = bids.appendOrder(lo)))
    def removeBest(tpe: OrderType): OrderBook = tpe.askBid(copy(asks = asks.withoutBest), copy(bids = bids.withoutBest))
    def replaceBestBy(lo: LimitOrder): OrderBook = removeBest(lo.order.tpe).append(lo)

    def clientsPortfolio: ClientsPortfolio = collectClientsPortfolio(all)
    private def collectClientsPortfolio(xs: Iterable[LimitOrder]): ClientsPortfolio =
      Monoid[ClientsPortfolio].combineAll(xs.map(_.clientSpend))
  }
}
