package org.github.vsuharnikov.wavesexchange.logic

import cats.implicits._
import cats.kernel.Group
import org.github.vsuharnikov.wavesexchange.collections.MapOps
import org.github.vsuharnikov.wavesexchange.domain.Implicits.SideOps
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.serde.DebugJson._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Properties}
import play.api.libs.json.Json
import play.api.libs.json.Json.toJson

object ExecuteSpecification extends Properties("logic.execute") with DomainGen {
  private val clientsNumber = 1L to 3L
  private val amounts = 1L to 1000L
  private val prices = 1L to 10000L
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

//  private val testGen = for {
//    order1 <- Gen.const(Order(OrderId(4), ClientId("C4"), OrderType.Bid, defaultPair, 5, 4))
//    order2 <- Gen.const(Order(OrderId(10), ClientId("C4"), OrderType.Ask, defaultPair, 3, 2))
//  } yield (order1, order2)

  private val overlappingGen = for {
    spreadStart <- Gen.choose(prices.start, prices.end - 1)
    spreadEnd <- Gen.choose(spreadStart + 1, prices.end)
    spread = spreadStart to spreadEnd
    maxAsk <- Gen.choose(spreadEnd, Int.MaxValue)
    orderTpe <- Gen.oneOf(OrderType.Ask, OrderType.Bid)
    orderBook <- orderBookGen(defaultPair, spread, maxAsk, amounts).filterNot { ob =>
      orderTpe.askBid(ob.bids.isEmpty, ob.asks.isEmpty)
    }
    (newOrderPrices, amounts) = orderTpe.askBid(
      (orderBook.bids.best, orderBook.bids.worst) match {
        case (Some(t), Some(f)) => (f.order.pricePerOne to t.order.pricePerOne, 1L to orderBook.bids.minAmount)
        case _                  => throw new IllegalStateException("Impossibru!")
      },
      (orderBook.asks.best, orderBook.asks.worst) match {
        case (Some(f), Some(t)) => (f.order.pricePerOne to t.order.pricePerOne, 1L to orderBook.asks.minAmount)
        case _                  => throw new IllegalStateException("Impossibru!")
      }
    )
    orderClient <- Gen.choose(clientsNumber.start, clientsNumber.end)
    order <- orderGen(OrderId(Int.MaxValue), ClientId(s"$orderClient"), defaultPair, orderTpe, newOrderPrices, amounts)
  } yield (orderBook, order)

  // // For test debugging purposes
  private val fixedTestGen = {
//      val rawOrderBook =
//        """{"a":[{"order":{"id":1,"client":"1","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":328761,"amount":546},"restAmount":546},{"order":{"id":4,"client":"1","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":994980,"amount":379},"restAmount":379},{"order":{"id":3,"client":"3","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":6469780,"amount":96},"restAmount":96},{"order":{"id":5,"client":"2","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":8766742,"amount":532},"restAmount":532},{"order":{"id":2,"client":"1","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":22767868,"amount":10},"restAmount":10}],"b":[{"order":{"id":1,"client":"1","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":328761,"amount":546},"restAmount":546},{"order":{"id":4,"client":"1","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":994980,"amount":379},"restAmount":379},{"order":{"id":3,"client":"3","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":6469780,"amount":96},"restAmount":96},{"order":{"id":5,"client":"2","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":8766742,"amount":532},"restAmount":532},{"order":{"id":2,"client":"1","tpe":"Ask","pair":{"amountId":"A","priceId":"B"},"pricePerOne":22767868,"amount":10},"restAmount":10}]}""".stripMargin
    val rawOrderBook =
      """{
          |  "a": [
          |    {
          |      "order": {
          |        "id": 2,
          |        "client": "2",
          |        "tpe": "Ask",
          |        "pair": {
          |          "amountId": "A",
          |          "priceId": "B"
          |        },
          |        "pricePerOne": 936629251,
          |        "amount": 480
          |      },
          |      "restAmount": 480
          |    }
          |  ],
          |  "b": []
          |}""".stripMargin
    val rawOrder =
      """{
          |  "id": 2147483647,
          |  "client": "2",
          |  "tpe": "Bid",
          |  "pair": {
          |    "amountId": "A",
          |    "priceId": "B"
          |  },
          |  "pricePerOne": 1100988370,
          |  "amount": 300
          |}""".stripMargin

    for {
      orderBook <- Gen.const[OrderBook](Json.parse(rawOrderBook).as[OrderBook])
      order <- Gen.const[Order](Json.parse(rawOrder).as[Order])
    } yield (orderBook, order)
  }

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

  property("assets invariant") = forAll(fixedTestGen) {
    case (obOrig, order) =>
      val aOrig = obOrig.clientsPortfolio |+| order.clientSpend
      val rOrig = obOrig.clientsPortfolio

      val (obUpdated, events) = append(obOrig, LimitOrder(order))
      val (aDiff, rDiff) = foldEvents(events)

      val aUpdated = aOrig |+| ClientsPortfolio(aDiff)
      val rUpdated = rOrig |+| ClientsPortfolio(rDiff)

      val assetsBefore = countAssets(aOrig.p)
      val assetsAfter = countAssets(aUpdated.p)

      val eventsDiffs = events.flatMap {
        case evt @ OrderEvent.Executed(maker, taker) => Some(evt -> execute(maker, taker))
        case _                                       => None
      }

      s"""|assets before:
          |$assetsBefore
          |assets after:
          |$assetsAfter
          |aOrig:
          |$aOrig
          |aUpdated:
          |$aUpdated
          |rOrig:
          |$rOrig
          |rUpdated:
          |$rUpdated
          |obOrig:
          |${toJson(obOrig)}
          |obUpdated:
          |${toJson(obUpdated)}
          |order:
          |${toJson(order)}
          |events:
          |${events.mkString("\n")}
          |eventsDiffs:
          |${eventsDiffs.map{case (evt, (aDiff, rDiff)) => s"  evt: $evt\n  aDiff: $aDiff\n  rDiff: $rDiff"}.mkString("\n")}
          |obOrig.clientsPortfolio:
          |${obOrig.clientsPortfolio}
          |order.clientSpend:
          |${order.clientSpend}
          |""".stripMargin |: assetsBefore.p.toSet == assetsAfter.p.toSet
  }

//  property("assets invariant") = forAll(testGen) {
//    case (order1, order2) =>
//      val (aPortDiff, rPortDiff) = execute(LimitOrder(order1), LimitOrder(order2))
//      val coinsDiff = countAssets(aPortDiff, OrderBook.empty).p.filter(_._2 != 0)
//
//      s"""coins diff:
//         |$coinsDiff
//         |order1
//         |${toJson(order1)}
//         |order2:
//         |${toJson(order2)}
//         |active portfolio diff:
//         |$aPortDiff
//         |reserved portfolio diff:
//         |$rPortDiff""".stripMargin |: (coinsDiff.isEmpty && rPortDiff.values.flatMap(_.p.values).forall(_ < 0))
//  }

}
