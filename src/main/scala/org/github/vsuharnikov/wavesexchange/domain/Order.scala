package org.github.vsuharnikov.wavesexchange.domain

case class Order(client: ClientId, tpe: OrderType, pair: AssetPair, pricePerOne: Int, amount: AssetAmount) {
  override def toString: String = s"Order($client, $tpe, $pair, ppo=$pricePerOne, a=$amount)"
}

object Order {
  final implicit class Ops(val self: Order) extends AnyVal {
    def clientSpend: ClientsPortfolio = ClientsPortfolio(Map(self.client -> spend))

    def spend: Portfolio = spend(self.pricePerOne, self.amount)
    def spend(executedPricePerOne: Int, executedAmount: Int): Portfolio = Portfolio {
      if (self.tpe == OrderType.Ask) Map(self.pair.amountId -> -executedAmount)
      else Map(self.pair.priceId -> -executedAmount * executedPricePerOne)
    }

    def receive: Portfolio = receive(self.pricePerOne, self.amount)
    def receive(executedPricePerOne: Int, executedAmount: Int): Portfolio = Portfolio {
      if (self.tpe == OrderType.Ask) Map(self.pair.priceId -> executedAmount * executedPricePerOne)
      else Map(self.pair.amountId -> executedAmount)
    }
  }
}
