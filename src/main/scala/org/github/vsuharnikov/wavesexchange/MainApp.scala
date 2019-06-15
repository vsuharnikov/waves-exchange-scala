package org.github.vsuharnikov.wavesexchange

import java.io.File

import cats.kernel.{Group, Monoid}
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv

import scala.io.Source

object MainApp {
  def main(args: Array[String]): Unit = {
    val baseDir = args.headOption.getOrElse(
      throw new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))

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
        val (finalObs, finalPort) =
          orders.zipWithIndex.foldLeft((Map.empty[AssetPair, OrderBook], clients)) {
            case (curr @ (currObs, origAllPort), (order, i)) =>
              val clientPort = origAllPort.p.getOrElse(order.client, Monoid.empty[Portfolio])
              validate(order, clientPort) match {
                case Left(e) =>
                  System.err.println(s"[$i] Can't apply $order: $e")
                  curr

                case Right(_) =>
                  val ob = currObs.getOrElse(order.pair, OrderBook.empty)
                  val (updatedOb, updatedAllPort) = append(ob, order, origAllPort)
                  (currObs.updated(order.pair, updatedOb), updatedAllPort)
              }
          }

        val obPortfolio = Group[ClientsPortfolio].inverse(Monoid.combineAll(finalObs.values.map(_.clientsPortfolio)))
        val finalClients = finalPort |+| obPortfolio
        val assetsAfter = Monoid.combineAll(finalClients.values)

        println(show"""Assets before:
                      |${countAssets(clients, OrderBook.empty)}
                      |Assets after:
                      |$assetsAfter
                      |Clients portfolio before:
                      |$clients
                      |Clients portfolio after:
                      |$finalClients"""".stripMargin)
        println(s"Final order books:\n${finalObs.mkString("\n")}")
    }
  }
}
