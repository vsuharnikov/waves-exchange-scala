package org.github.vsuharnikov.wavesexchange

import cats.kernel.{Group, Monoid}
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import org.github.vsuharnikov.wavesexchange.domain.{AssetId, AssetPair, ClientsPortfolio, Order, OrderBook}
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv
import zio.console._
import zio.nio.file.{Files, Path}
import zio.stm.TQueue
import zio.{App, Task, ZIO}

import scala.collection.immutable.Queue
import scala.reflect.ClassTag

object MainApp extends App {
  override def run(args: List[String]) =
    extractArguments(args)
      .flatMap(logic)
      .tapError { e =>
        putStrLn(e.getMessage)
      }
      .fold(_ => 1, _ => 0)

  private def extractArguments(args: List[String]): Task[Arguments] =
    if (args.isEmpty) Task.fail(new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))
    else Task(Arguments(Path(args.head)))

  type OrderBookQueues = Map[AssetPair, TQueue[Order]]

  private def logic(args: Arguments) =
    for {
      initialClientsPortfolio <- Files.readAllLines(args.outputDir / "clients.txt").flatMap { xs =>
        ZIO.fromEither(Csv.clients(xs, assets)).absorbWith(new ParseException(_))
      }
      // todo stream
      orders <- Files.readAllLines(args.outputDir / "orders.txt").map { xs =>
        xs.map(Csv.order).collect {
          case Right(x) => x
        }
      }
      process <- ()
      _ <- ZIO.foreach(orders)()
      _ <- {
        val (updatedClientsPortfolio, finalObs, invalidOrders) = mainLoop(initialClientsPortfolio, orders)

        val obPortfolio = Group[ClientsPortfolio].inverse(Monoid.combineAll(finalObs.values.map(_.clientsPortfolio)))
        val finalClientsPortfolio = updatedClientsPortfolio |+| obPortfolio
        val assetsAfter = Monoid.combineAll(finalClientsPortfolio.values)

        putStrLn(
          show"""Assets before:
              |${countAssets(initialClientsPortfolio, OrderBook.empty)}
              |Assets after:
              |$assetsAfter
              |Clients portfolio before:
              |$initialClientsPortfolio
              |Clients portfolio after:
              |$finalClientsPortfolio""".stripMargin
        ) *>
          putStrLn(s"Final order books:\n${finalObs.mkString("\n")}") *>
          putStrLn(s"Invalid orders:\n${invalidOrders
            .map { x =>
              s"[${x.index}] ${x.order}: ${x.reason}"
            }
            .mkString("\n")}")
      }
    } yield Unit

  private case class Arguments(outputDir: Path)

  // https://github.com/estatico/scala-newtype/issues/10
  private val assets = List('$', 'A', 'B', 'C', 'D').map(AssetId(_)).toArray(implicitly[ClassTag[Char]].asInstanceOf[ClassTag[AssetId]])

  private def mainLoop(clients: ClientsPortfolio, orders: Iterable[Order]): (ClientsPortfolio, Map[AssetPair, OrderBook], Queue[InvalidOrder]) =
    orders.zipWithIndex.foldLeft((clients, Map.empty[AssetPair, OrderBook], Queue.empty[InvalidOrder])) {
      case (curr @ (origAllPort, currObs, invalidOrders), (order, i)) =>
        validate(order, origAllPort(order.client)) match {
          case Left(e) => curr.copy(_3 = invalidOrders.enqueue(InvalidOrder(i, order, e)))
          case Right(_) =>
            val ob = currObs.getOrElse(order.pair, OrderBook.empty)
            val (updatedAllPort, updatedOb) = append(ob, order, origAllPort)
            (updatedAllPort, currObs.updated(order.pair, updatedOb), invalidOrders)
        }
    }

  private case class InvalidOrder(index: Int, order: Order, reason: String)
}

class ParseException(message: String) extends RuntimeException(message)
