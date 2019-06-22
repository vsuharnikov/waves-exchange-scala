package org.github.vsuharnikov.wavesexchange.logic

import cats.implicits._
import cats.kernel.Group
import org.github.vsuharnikov.wavesexchange.collections.MapOps
import org.github.vsuharnikov.wavesexchange.domain.Implicits.SideOps
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.serde.DebugJson._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Properties}
import play.api.libs.json.Json.toJson

object AppendSpecification extends Properties("logic.append") {
  private val clientsNumber = 1 to 3
  private val amounts = 1 to 1000
  private val prices = 1 to 10000
  private val defaultPair = AssetPair(AssetId('A'), AssetId('B'))

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
      orderTpe.askBid(ob.bids.isEmpty, ob.asks.isEmpty)
    }
    (newOrderPrices, amounts) = orderTpe.askBid(
      (orderBook.bids.best, orderBook.bids.worst) match {
        case (Some(t), Some(f)) => (f.pricePerOne to t.pricePerOne, 1 to orderBook.bids.minAmount)
        case _                  => throw new IllegalStateException("Impossibru!")
      },
      (orderBook.asks.best, orderBook.asks.worst) match {
        case (Some(f), Some(t)) => (f.pricePerOne to t.pricePerOne, 1 to orderBook.asks.minAmount)
        case _                  => throw new IllegalStateException("Impossibru!")
      }
    )
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

  property("submitter receives >= expected") = forAll(overlappingGen) {
    case (origOb, order) =>
      val (updatedPortfolio, _) = append(origOb, order)

      val toCheck = order.receive.p.zipMap(updatedPortfolio.p(order.client).p)
      val toCheckStr = toCheck
        .map { case (assetId, (expected, actual)) => s"exp. $expected vs act. $actual $assetId" }
        .mkString("\n")

      val prop = toCheck.forall { case (_, (expected, actual)) => actual >= expected }

      s"""original order book:
         |${toJson(origOb)}
         |order:
         |${toJson(order)}
         |to check:
         |$toCheckStr""".stripMargin |: prop
  }

  property("assets invariant") = forAll(testGen) {
    case (origOb, order) =>
      val origPort = Group[ClientsPortfolio].inverse(origOb.clientsPortfolio |+| order.clientSpend)
      val (updatedPort, updatedOb) = append(origOb, order, origPort)

      val assetsBefore = countAssets(origPort, origOb)
      val assetsAfter = countAssets(updatedPort, updatedOb)

      s"""original portfolio:
         |$origPort
         |original order book:
         |${toJson(origOb)}
         |order:
         |${toJson(order)}
         |assets before:
         |$assetsBefore
         |assets after:
         |$assetsAfter""".stripMargin |: assetsBefore.p.toSet == assetsAfter.p.toSet
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

  private def clientIdGen: Gen[ClientId] =
    Gen.choose(clientsNumber.start, clientsNumber.end).map(x => ClientId(x.toString))

}
