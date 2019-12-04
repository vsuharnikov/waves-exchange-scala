package org.github.vsuharnikov.wavesexchange

import _root_.io.estatico.newtype.macros.newtype
import _root_.io.estatico.newtype.ops._
import cats.instances.int.catsKernelStdGroupForInt
import cats.instances.map.catsKernelStdMonoidForMap
import cats.syntax.show.showInterpolator
import cats.{Group, Monoid, Show}
import org.github.vsuharnikov.wavesexchange.collections.groupForMap

import scala.collection.immutable.{Queue, TreeMap}

// For simplicity there is one file for simple models
package object domain {
  private implicit val intShow: Show[Int] = _.toString
  private val charShow: Show[Char] = _.toString
  private val strShow: Show[String] = x => x

  @newtype case class AssetId(id: Char)
  object AssetId {
    implicit val show: Show[AssetId] = charShow.coerce
  }

  val Dollar = AssetId('$')

  case class AssetPair(amountId: AssetId, priceId: AssetId)
  object AssetPair {

    final implicit class Ops(val self: AssetPair) extends AnyVal {
      def assets: List[AssetId] = List(self.amountId, self.priceId)
    }
  }

  type AssetPrice = Int
  type AssetAmount = Int

  // newtype, so we don't need to import instances for Map everywhere
  @newtype case class Portfolio(p: Map[AssetId, AssetAmount])
  object Portfolio {
    implicit val group: Group[Portfolio] = groupForMap[AssetId, AssetAmount].coerce
    // overriding toString doesn't work for newtypes
    implicit val show: Show[Portfolio] = _.p.toVector.sortBy(_._1.id).map { case (assetId, amount) => show"$amount $assetId" }.mkString(", ")
    def apply(pair: (AssetId, AssetAmount)): Portfolio = Portfolio(Map(pair))
  }

  @newtype case class ClientId(id: String)
  object ClientId {
    implicit val show: Show[ClientId] = strShow.coerce
  }

  @newtype case class ClientsPortfolio(p: Map[ClientId, Portfolio]) {
    def values: Iterable[Portfolio] = p.values
    def apply(id: ClientId): Portfolio = p.getOrElse(id, Monoid[Portfolio].empty)
  }
  object ClientsPortfolio {
    implicit val group: Group[ClientsPortfolio] = groupForMap[ClientId, Portfolio].coerce
    implicit val show: Show[ClientsPortfolio] = _.p.toVector.sortBy(_._1.id).map { case (clientId, p) => show"$clientId: $p" }.mkString("\n")
    def apply(pair: (ClientId, Portfolio)*): ClientsPortfolio = ClientsPortfolio(Monoid.combineAll(pair.toIterable.map(Map(_))))
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

    private val asksPriceOrder: Ordering[AssetPrice] = Ordering.apply
    private val bidsPriceOrder: Ordering[AssetPrice] = asksPriceOrder.reverse

    def empty(tpe: OrderType): Side = Side(TreeMap.empty(tpe.askBid(asksPriceOrder, bidsPriceOrder)))

    private def appendOrder(orders: Orders, x: Order): Orders =
      orders.updated(x.pricePerOne, orders.getOrElse(x.pricePerOne, Queue.empty).enqueue(x))
  }
}
