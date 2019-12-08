package org.github.vsuharnikov.wavesexchange.serde

import io.estatico.newtype.ops._
import org.github.vsuharnikov.wavesexchange.domain._
import play.api.libs.json._

object DebugJson {
  private val strFormat: Format[String] = Format(
    {
      case JsString(x) => JsSuccess(x)
      case x           => JsError(s"Can't read '$x' as string")
    },
    JsString(_)
  )

  private val charFormat: Format[Char] = Format(
    strFormat.collect(JsonValidationError("Required a char, but provided a string")) { case x if x.length == 1 => x.head },
    strFormat.contramap(_.toString)
  )

  implicit val orderId: Format[OrderId] = Format(implicitly[Reads[Int]], implicitly[Writes[Int]]).coerce[Format[OrderId]]
  implicit val assetId: Format[AssetId] = charFormat.coerce[Format[AssetId]]
  implicit val assetPair: Format[AssetPair] = Json.format[AssetPair]

  implicit val clientId: Format[ClientId] = strFormat.coerce[Format[ClientId]]

  implicit val orderTypeJson: Format[OrderType] = Format[OrderType](
    {
      case JsString(x) =>
        x match {
          case "Ask" => JsSuccess(OrderType.Ask)
          case "Bid" => JsSuccess(OrderType.Bid)
          case _     => JsError(s"Can't parse as OrderType: JsString($x)")
        }
      case x => JsError(s"Can't parse as OrderType: $x")
    },
    x => JsString(x.askBid("Ask", "Bid"))
  )

  implicit val orderJson: Format[Order] = Json.format[Order]
  implicit val limitOrderJson: Format[LimitOrder] = Json.format[LimitOrder]
  implicit val orderBookJson: Format[OrderBook] = Format[OrderBook](
    {
      case JsObject(obj) =>
        val r = for {
          asks <- obj.get("a").map(_.as[List[LimitOrder]])
          bids <- obj.get("b").map(_.as[List[LimitOrder]])
        } yield OrderBook(asks, bids)

        r.fold[JsResult[OrderBook]](JsError(s"Can't parse as OrderBook: JsObject($obj)"))(JsSuccess(_))
      case x => JsError(s"Can't parse as OrderBook: $x")
    },
    orderBook => Json.obj("a" -> orderBook.asks.allOrders, "b" -> orderBook.asks.allOrders)
  )
}
