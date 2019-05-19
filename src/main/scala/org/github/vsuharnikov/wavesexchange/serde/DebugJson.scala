package org.github.vsuharnikov.wavesexchange.serde

import org.github.vsuharnikov.wavesexchange.domain._
import play.api.libs.json._

object DebugJson {
  implicit val orderTypeJson: Format[OrderType] = Format[OrderType](
    {
      case JsString(x) =>
        x match {
          case "Ask" => JsSuccess(OrderType.Ask)
          case "Bid" => JsSuccess(OrderType.Bid)
          case _     => JsError(s"Can't parse as OrderType: JsString($x)")
        }
      case x => JsError(s"Can't parse as OrderType: $x")
    }, {
      case OrderType.Ask => JsString("Ask")
      case OrderType.Bid => JsString("Bid")
    }
  )

  implicit val orderJson: Format[Order] = Json.format[Order]
  implicit val orderBookJson: Format[OrderBook] = Format[OrderBook](
    {
      case JsObject(obj) =>
        val r = for {
          asks <- obj.get("a").map(_.as[List[Order]])
          bids <- obj.get("b").map(_.as[List[Order]])
        } yield OrderBook(asks, bids)

        r.fold[JsResult[OrderBook]](JsError(s"Can't parse as OrderBook: JsObject($obj)"))(JsSuccess(_))
      case x => JsError(s"Can't parse as OrderBook: $x")
    },
    orderBook => Json.obj("a" -> orderBook.asks.side.toList, "b" -> orderBook.asks.side.toList)
  )
}
