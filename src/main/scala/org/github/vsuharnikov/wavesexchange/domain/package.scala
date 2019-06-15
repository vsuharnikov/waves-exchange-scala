package org.github.vsuharnikov.wavesexchange

import cats.instances.int.catsKernelStdGroupForInt
import cats.instances.map.catsKernelStdMonoidForMap
import cats.syntax.group._
import cats.syntax.show.showInterpolator
import cats.{Group, Monoid, Show}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import play.api.libs.json._

import scala.collection.immutable.{Queue, TreeMap}

// For simplicity there is one file for simple models
package object domain {
  private val strShow: Show[String] = x => x
  private implicit val intShow: Show[Int] = _.toString
  private val strFormat: Format[String] = Format(
    {
      case JsString(x) => JsSuccess(x)
      case x           => JsError(s"Can't read '$x' as string")
    },
    JsString(_)
  )

  @newtype case class AssetId(id: String)
  object AssetId {
    implicit val show: Show[AssetId] = strShow.coerce
    implicit val json: Format[AssetId] = strFormat.coerce
  }

  val Dollar = AssetId("$")

  case class AssetPair(amountId: AssetId, priceId: AssetId)
  object AssetPair {
    implicit val json: Format[AssetPair] = Json.format[AssetPair]

    final implicit class Ops(val self: AssetPair) extends AnyVal {
      def assets: List[AssetId] = List(self.amountId, self.priceId)
    }
  }

  type AssetPrice = Int
  type AssetAmount = Int

  @newtype case class Portfolio(p: Map[AssetId, AssetAmount])
  object Portfolio {
    implicit val group: Group[Portfolio] = new Group[Portfolio] {
      private val monoid: Monoid[Portfolio] = catsKernelStdMonoidForMap[AssetId, AssetAmount].coerce
      override def inverse(a: Portfolio): Portfolio = Portfolio(a.p.map { case (k, v) => k -> -v })
      override def empty: Portfolio = monoid.empty
      override def combine(x: Portfolio, y: Portfolio): Portfolio = monoid.combine(x, y)
    }
    implicit val show: Show[Portfolio] = _.p.map { case (assetId, amount) => show"$amount $assetId" }.mkString(", ")
    def apply(pair: (AssetId, AssetAmount)): Portfolio = Portfolio(Map(pair))
  }

  @newtype case class ClientId(id: String)
  object ClientId {
    implicit val show: Show[ClientId] = strShow.coerce
    implicit val json: Format[ClientId] = strFormat.coerce
  }

  @newtype case class ClientsPortfolio(p: Map[ClientId, Portfolio]) {
    def values: Iterable[Portfolio] = p.values
  }
  object ClientsPortfolio {
    implicit val group: Group[ClientsPortfolio] = new Group[ClientsPortfolio] {
      private val monoid: Monoid[ClientsPortfolio] = catsKernelStdMonoidForMap[ClientId, Portfolio].coerce
      override def inverse(a: ClientsPortfolio): ClientsPortfolio =
        ClientsPortfolio(a.p.map { case (k, v) => k -> Portfolio.group.inverse(v) })
      override def empty: ClientsPortfolio = monoid.empty
      override def combine(x: ClientsPortfolio, y: ClientsPortfolio): ClientsPortfolio = monoid.combine(x, y)
    }

    implicit val show: Show[ClientsPortfolio] =
      _.p.toVector.sortBy(_._1.id).map { case (clientId, p) => show"$clientId: $p" }.mkString("\n")
    def apply(pair: (ClientId, Portfolio)*): ClientsPortfolio = {
      val xs = pair.foldLeft(Map.empty[ClientId, Portfolio]) {
        case (r, (id, p)) => r.updated(id, r.getOrElse(id, Monoid[Portfolio].empty) |+| p)
      }
      ClientsPortfolio(xs)
    }
  }

  import Side.Orders

  @newtype case class Side(orders: Orders) {
    def appendOrder(x: Order): Side = Side(Side.appendOrder(orders, x))
    def appendOrders(xs: Iterable[Order]): Side = Side(xs.foldLeft(orders)(Side.appendOrder))
    def withoutBest: Side = Side {
      if (orders.isEmpty) orders
      else {
        val (price, q) = orders.head
        if (q.lengthCompare(1) == 0) orders.tail
        else orders.updated(price, q.tail)
      }
    }

    def isEmpty: Boolean = orders.isEmpty
    def best: Option[Order] = orders.headOption.flatMap(_._2.headOption)
    def allOrders: Iterable[Order] = orders.values.flatten
  }

  object Side {
    type Orders = TreeMap[AssetPrice, Queue[Order]]

    val asksPriceOrder: Ordering[AssetPrice] = Ordering.apply
    val bidsPriceOrder: Ordering[AssetPrice] = asksPriceOrder.reverse

    def empty(tpe: OrderType): Side = Side(TreeMap.empty(if (tpe == OrderType.Ask) asksPriceOrder else bidsPriceOrder))

    private def appendOrder(orders: Orders, x: Order): Orders =
      orders.updated(x.pricePerOne, orders.getOrElse(x.pricePerOne, Queue.empty).enqueue(x))
  }
}
