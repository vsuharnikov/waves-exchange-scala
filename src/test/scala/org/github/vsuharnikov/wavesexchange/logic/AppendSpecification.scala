package org.github.vsuharnikov.wavesexchange.logic

import cats.implicits._
import cats.kernel.Monoid
import org.github.vsuharnikov.wavesexchange.collections.MapOps
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.serde.DebugJson._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Properties}
import play.api.libs.json.Json.toJson

object AppendSpecification extends Properties("logic.append") {
  private val clientsNumber = 1 to 3
  private val amounts = 1 to 1000
  private val prices = 1 to 10000
  private val defaultPair = AssetPair(AssetId("A"), AssetId("B"))

  private val testGen = for {
    spreadStart <- Gen.choose(prices.start, prices.end - 1)
    spreadEnd <- Gen.choose(spreadStart + 1, prices.end)
    spread = spreadStart to spreadEnd
    maxAsk <- Gen.choose(spreadEnd, Int.MaxValue)
    orderBook <- orderBookGen(defaultPair, spread, maxAsk)
    orderTpe <- Gen.oneOf(OrderType.Ask, OrderType.Bid)
    order <- orderGen(defaultPair, orderTpe, prices)
  } yield (orderBook, order)

  private val overlappingGen = for {
    spreadStart <- Gen.choose(prices.start, prices.end - 1)
    spreadEnd <- Gen.choose(spreadStart + 1, prices.end)
    spread = spreadStart to spreadEnd
    maxAsk <- Gen.choose(spreadEnd, Int.MaxValue)
    orderTpe <- Gen.oneOf(OrderType.Ask, OrderType.Bid)
    orderBook <- orderBookGen(defaultPair, spread, maxAsk).filterNot { ob =>
      orderTpe match {
        case OrderType.Ask => ob.bids.side.isEmpty
        case OrderType.Bid => ob.asks.side.isEmpty
      }
    }
    (newOrderPrices, amounts) = orderTpe match {
      case OrderType.Ask =>
        (orderBook.bids.side.headOption, orderBook.bids.side.lastOption) match {
          case (Some(t), Some(f)) => (f.pricePerOne to t.pricePerOne, 1 to orderBook.bids.side.map(_.amount).min)
          case _                  => throw new IllegalStateException("Impossibru!")
        }
      case OrderType.Bid =>
        (orderBook.asks.side.headOption, orderBook.asks.side.lastOption) match {
          case (Some(f), Some(t)) => (f.pricePerOne to t.pricePerOne, 1 to orderBook.asks.side.map(_.amount).min)
          case _                  => throw new IllegalStateException("Impossibru!")
        }
    }
    order <- orderGen(defaultPair, orderTpe, newOrderPrices, amounts)
  } yield (orderBook, order)

  // // For test debugging purposes
  //  private val fixedTestGen = {
  //    val rawOrderBook =
  //      """{"a":[{"client":"1","tpe":"Ask","pair":{"amountId":{"name":"A"},"priceId":{"name":"B"}},"pricePerOne":230119358,"amount":318}],"b":[{"client":"1","tpe":"Ask","pair":{"amountId":{"name":"A"},"priceId":{"name":"B"}},"pricePerOne":230119358,"amount":318}]}"""
  //    val rawOrder =
  //      """{"client":"3","tpe":"Ask","pair":{"amountId":{"name":"A"},"priceId":{"name":"B"}},"pricePerOne":861986,"amount":471}"""
  //
  //    for {
  //      orderBook <- Gen.const[OrderBook](Json.parse(rawOrderBook).as[OrderBook])
  //      order <- Gen.const[Order](Json.parse(rawOrder).as[Order])
  //    } yield (orderBook, order)
  //  }

  property("all receive only a positive amount of assets") = forAll(testGen) {
    case (origOb, order) =>
      val (_, updatedPortfolio) = append(origOb, order)

      s"original order book:\n${toJson(origOb)}\norder:\n${toJson(order)}\nportfolio:\n$updatedPortfolio" |:
        updatedPortfolio.p.values.flatMap(_.p.values).forall(_ >= 0)
  }

  property("submitter receives >= expected") = forAll(overlappingGen) {
    case (origOb, order) =>
      val (_, updatedPortfolio) = append(origOb, order)

      val toCheck = order.receive.p.zipMap(updatedPortfolio.p(order.client).p)
      val toCheckStr = toCheck
        .map { case (assetId, (expected, actual)) => s"exp. $expected vs act. $actual $assetId" }
        .mkString("\n")

      val prop = toCheck.forall { case (_, (expected, actual)) => actual >= expected }

      s"original order book:\n${toJson(origOb)}\norder:\n${toJson(order)}\nto check:\n$toCheckStr" |:
        prop
  }

  property("coins invariant") = forAll(testGen) {
    case (origOb, order) =>
      val origPortfolio = createPortfolio(origOb.clientsPortfolio |+| order.clientSpend)
      val coinsBefore = countCoins(origPortfolio)

      val (updatedOb, updatedPortfolio) = append(origOb, order)
      val coinsAfter = countCoins(createPortfolio(updatedOb.clientsPortfolio) |+| updatedPortfolio)

      s"original order book:\n${toJson(origOb)}\norder:\n${toJson(order)}\ncoins before:\n$coinsBefore\ncoins after:\n$coinsAfter" |:
        coinsBefore.p.toSet == coinsAfter.p.toSet
  }

  private def orderBookGen(pair: AssetPair, spread: Range, maxAsk: Int): Gen[OrderBook] = {
    require(spread.start >= 1, "spread.start")
    require(spread.end > spread.start, "spread.end")
    require(maxAsk >= spread.end, "maxAsk")

    for {
      asks <- Gen.containerOf[List, Order](orderGen(pair, OrderType.Ask, spread.end to maxAsk))
      bids <- Gen.containerOf[List, Order](orderGen(pair, OrderType.Bid, 1 to spread.start))
    } yield OrderBook(asks, bids)
  }

  private def orderGen(pair: AssetPair, tpe: OrderType, price: Range, amounts: Range = amounts): Gen[Order] =
    for {
      clientId <- clientIdGen
      pricePerOne <- Gen.choose(price.start, price.end)
      amount <- Gen.choose(amounts.start, amounts.end)
    } yield Order(clientId, tpe, pair, pricePerOne, amount)

  private def clientIdGen: Gen[ClientId] = Gen.choose(clientsNumber.start, clientsNumber.end).map(x => ClientId(x.toString))

  private def countCoins(xs: ClientsPortfolio): Portfolio = xs.p.values.fold(Monoid[Portfolio].empty)(_ |+| _)

  private def createPortfolio(xs: ClientsPortfolio): ClientsPortfolio =
    ClientsPortfolio(xs.p.map { case (k, v) => k -> minus(v) })

  private def minus(p: Portfolio): Portfolio = Portfolio(p.p.map { case (k, v) => k -> -v })
}
