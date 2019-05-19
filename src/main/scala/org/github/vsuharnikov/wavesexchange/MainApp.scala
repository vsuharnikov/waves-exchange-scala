package org.github.vsuharnikov.wavesexchange

import java.io.File

import cats.kernel.{Group, Monoid}
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv

import scala.io.Source

object MainApp extends App {
  val baseDir = "/Users/freezy/projects/private/job-test/waves-exchange"
  val assets = Dollar :: ('A' to 'D').map(x => AssetId(x.toString)).toList

  val clientsFile = Source.fromFile(new File(s"$baseDir/clients.txt"))
  val ordersFile = Source.fromFile(new File(s"$baseDir/orders.txt"))

  val input = for {
    clients <- Csv.clients(clientsFile.getLines().toList, assets)
    orders <- Csv.orders(ordersFile.getLines().toList)
  } yield (clients, orders)

  input match {
    case Left(e) => System.err.println(e)
    case Right((clients, orders)) =>
      val (finalOrderBooks, executedPortfolio) =
        orders.zipWithIndex.foldLeft((Map.empty[AssetPair, OrderBook], clients)) {
          case (curr @ (currOrderBooks, allPortfolio), (order, i)) =>
            val clientPortfolio = allPortfolio.p.getOrElse(order.client, Monoid.empty[Portfolio])
            validate(order, clientPortfolio) match {
              case Left(e) =>
                System.err.println(s"[$i] Can't apply $order: $e")
                curr

              case Right(_) =>
                val reservedAllPortfolio = allPortfolio |+| ClientsPortfolio(Map(order.client -> order.spend))

                val orderBook = currOrderBooks.getOrElse(order.pair, OrderBook.empty)
                val (updatedOrderBook, portfolioChanges) = append(orderBook, order)

                (currOrderBooks.updated(order.pair, updatedOrderBook), reservedAllPortfolio |+| portfolioChanges)
            }
        }

      println(s"Final order books:\n${finalOrderBooks.mkString("\n")}")
      println(show"Executed portfolio:\n$executedPortfolio")

      // Cancel
      val finalPortfolio = executedPortfolio |+|
        Monoid.combineAll(finalOrderBooks.values.map(_.clientsPortfolio).map(Group[ClientsPortfolio].inverse))

      println(show"Final portfolio:\n$finalPortfolio")
  }
}
