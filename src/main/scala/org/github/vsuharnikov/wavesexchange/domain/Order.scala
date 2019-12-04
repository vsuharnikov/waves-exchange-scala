package org.github.vsuharnikov.wavesexchange.domain

import cats.Show

case class Order(id: OrderId, client: ClientId, tpe: OrderType, pair: AssetPair, pricePerOne: AssetPrice, amount: AssetAmount) {
  override def toString: String = s"Order($id, $client, $tpe, $pair, ppo=$pricePerOne, a=$amount)"
}

object Order {
  implicit val show: Show[Order] = _.toString

  final implicit class Ops(val self: Order) extends AnyVal {
    import self._

    def clientSpend: ClientsPortfolio = ClientsPortfolio(client -> spend)

    def spend: Portfolio = spend(pricePerOne, amount)
    def spend(executedPricePerOne: Int, executedAmount: Int): Portfolio = Portfolio {
      tpe.askBid(pair.amountId -> -executedAmount, pair.priceId -> -executedAmount * executedPricePerOne)
    }

    def receive: Portfolio = receive(pricePerOne, amount)
    def receive(executedPricePerOne: Int, executedAmount: Int): Portfolio = Portfolio {
      tpe.askBid(pair.priceId -> executedAmount * executedPricePerOne, pair.amountId -> executedAmount)
    }
  }
}
