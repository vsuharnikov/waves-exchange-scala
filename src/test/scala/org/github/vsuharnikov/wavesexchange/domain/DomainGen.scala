package org.github.vsuharnikov.wavesexchange.domain

import org.scalacheck.Gen

trait DomainGen {
  def orderBookGen(pair: AssetPair, spread: Range, maxAsk: Int, amounts: Range): Gen[OrderBook] = {
    require(spread.start >= 1, "spread.start")
    require(spread.end > spread.start, "spread.end")
    require(maxAsk >= spread.end, "maxAsk")

    def asks(number: Int) =
      Gen.sequence[List[LimitOrder], LimitOrder]((1 to number).map { id =>
        Gen.choose(1, 3).flatMap { clientId =>
          orderGen(OrderId(id), ClientId(s"$clientId"), pair, OrderType.Ask, spread.end to maxAsk, amounts).map(LimitOrder(_))
        }
      })

    def bids(number: Int) =
      Gen.sequence[List[LimitOrder], LimitOrder]((1 to number).map { id =>
        Gen.choose(1, 3).flatMap { clientId =>
          orderGen(OrderId(id), ClientId(s"$clientId"), pair, OrderType.Bid, 1 to spread.start, amounts).map(LimitOrder(_))
        }
      })

    for {
      asksNumber <- Gen.choose(0, 5)
      bidsNumber <- Gen.choose(0, 5)
      asks <- asks(asksNumber)
      bids <- bids(bidsNumber)
    } yield OrderBook(asks, bids)
  }

  def orderGen(id: OrderId, clientId: ClientId, pair: AssetPair, tpe: OrderType, price: Range, amounts: Range): Gen[Order] =
    for {
      pricePerOne <- Gen.choose(price.start, price.end)
      amount <- Gen.choose(amounts.start, amounts.end)
    } yield Order(id, clientId, tpe, pair, pricePerOne, amount)
}
