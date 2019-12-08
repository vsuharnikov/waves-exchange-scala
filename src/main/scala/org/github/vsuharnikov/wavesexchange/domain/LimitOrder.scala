package org.github.vsuharnikov.wavesexchange.domain

case class LimitOrder(order: Order, restAmount: AssetAmount)

object LimitOrder {
  def apply(order: Order): LimitOrder = new LimitOrder(order, order.amount)

  final implicit class Ops(val self: LimitOrder) extends AnyVal {
    def clientSpend: ClientsPortfolio = ClientsPortfolio(self.order.client -> spend)
    def spend: Portfolio = self.order.spend(self.order.pricePerOne, self.restAmount)
  }
}
