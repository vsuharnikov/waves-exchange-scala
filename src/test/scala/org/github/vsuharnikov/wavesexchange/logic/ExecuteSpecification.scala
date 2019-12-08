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

object ExecuteSpecification extends Properties("logic.execute") with DomainGen {
  private val clientsNumber = 1 to 3
  private val amounts = 1 to 1000
  private val prices = 1 to 10000
  private val defaultPair = AssetPair(AssetId('A'), AssetId('B'))

  private val testGen = for {
    spreadStart <- Gen.choose(prices.start, prices.end - 1)
    spreadEnd <- Gen.choose(spreadStart + 1, prices.end)
    spread = spreadStart to spreadEnd
    order1Tpe <- Gen.oneOf(OrderType.Ask, OrderType.Bid)
    prices1 = prices
    order1 <- orderGen(OrderId(1), ClientId("C1"), defaultPair, order1Tpe, prices1, amounts)
    prices2 = order1Tpe.askBid(order1.pricePerOne to prices1.end, prices1.start to order1.pricePerOne)
    order2 <- orderGen(OrderId(2), ClientId("C2"), defaultPair, order1Tpe.opposite, prices2, amounts)
  } yield (order1, order2)

//  private val overlappingGen = for {
//    spreadStart <- Gen.choose(prices.start, prices.end - 1)
//    spreadEnd <- Gen.choose(spreadStart + 1, prices.end)
//    spread = spreadStart to spreadEnd
//    maxAsk <- Gen.choose(spreadEnd, Int.MaxValue)
//    orderTpe <- Gen.oneOf(OrderType.Ask, OrderType.Bid)
//    orderBook <- orderBookGen(defaultPair, spread, maxAsk).filterNot { ob =>
//      orderTpe.askBid(ob.bids.isEmpty, ob.asks.isEmpty)
//    }
//    (newOrderPrices, amounts) = orderTpe.askBid(
//      (orderBook.bids.best, orderBook.bids.worst) match {
//        case (Some(t), Some(f)) => (f.pricePerOne to t.pricePerOne, 1 to orderBook.bids.minAmount)
//        case _                  => throw new IllegalStateException("Impossibru!")
//      },
//      (orderBook.asks.best, orderBook.asks.worst) match {
//        case (Some(f), Some(t)) => (f.pricePerOne to t.pricePerOne, 1 to orderBook.asks.minAmount)
//        case _                  => throw new IllegalStateException("Impossibru!")
//      }
//    )
//    order <- orderGen(defaultPair, orderTpe, newOrderPrices, amounts)
//  } yield (orderBook, order)

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

//  property("submitter receives >= expected") = forAll(overlappingGen) {
//    case (origOb, order) =>
//      val (updatedPortfolio, _) = execute(origOb, order)
//
//      val toCheck = order.receive.p.zipMap(updatedPortfolio.p(order.client).p)
//      val toCheckStr = toCheck
//        .map { case (assetId, (expected, actual)) => s"exp. $expected vs act. $actual $assetId" }
//        .mkString("\n")
//
//      val prop = toCheck.forall { case (_, (expected, actual)) => actual >= expected }
//
//      s"""original order book:
//         |${toJson(origOb)}
//         |order:
//         |${toJson(order)}
//         |to check:
//         |$toCheckStr""".stripMargin |: prop
//  }

  property("assets invariant") = forAll(testGen) {
    case (order1, order2) =>
      val (aPortDiff, rPortDiff) = execute(LimitOrder(order1), LimitOrder(order2))
      val coinsDiff = countAssets(aPortDiff, OrderBook.empty).p.filter(_._2 != 0)

      s"""coins diff:
         |$coinsDiff
         |original portfolio:
         |$aPortDiff
         |order1
         |${toJson(order1)}
         |order2:
         |${toJson(order2)}
         |active portfolio diff:
         |$aPortDiff
         |reserved portfolio diff:
         |$rPortDiff""".stripMargin |: (coinsDiff.isEmpty && rPortDiff.values.flatMap(_.p.values).forall(_ < 0))
  }

}
