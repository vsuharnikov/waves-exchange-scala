package org.github.vsuharnikov.wavesexchange

import java.io.File

import cats.kernel.{Group, Monoid}
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv

import scala.io.Source
import scala.reflect.ClassTag

object MainApp {
  def main(args: Array[String]): Unit = {
    val baseDir =
      args.headOption.getOrElse(throw new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))

    // https://github.com/estatico/scala-newtype/issues/10
    val assets =
      List('$', 'A', 'B', 'C', 'D').map(AssetId(_)).toArray(implicitly[ClassTag[Char]].asInstanceOf[ClassTag[AssetId]])

    val clientsFile = Source.fromFile(new File(s"$baseDir/clients.txt"))
    val ordersFile = Source.fromFile(new File(s"$baseDir/orders.txt"))

    try {
      Csv.clients(clientsFile.getLines().toIterable, assets) match {
        case Left(e) => System.err.println(e)
        case Right(clients) =>
          val orders = ordersFile.getLines().toIterable.map(Csv.order).collect { case Right(x) => x }
          val (finalPort, finalObs) = mainLoop(clients, orders)

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
                        |$finalClients""".stripMargin)
          println(s"Final order books:\n${finalObs.mkString("\n")}")
      }
    } finally {
      clientsFile.close()
      ordersFile.close()
    }
  }

  def mainLoop(clients: ClientsPortfolio, orders: Iterable[Order]): (ClientsPortfolio, Map[AssetPair, OrderBook]) =
    orders.zipWithIndex.foldLeft((clients, Map.empty[AssetPair, OrderBook])) {
      case (curr @ (origAllPort, currObs), (order, i)) =>
        validate(order, origAllPort(order.client)) match {
          case Left(e) =>
            System.err.println(s"[$i] Can't apply $order: $e")
            curr

          case Right(_) =>
            val ob = currObs.getOrElse(order.pair, OrderBook.empty)
            val (updatedAllPort, updatedOb) = append(ob, order, origAllPort)
            (updatedAllPort, currObs.updated(order.pair, updatedOb))
        }
    }
}
