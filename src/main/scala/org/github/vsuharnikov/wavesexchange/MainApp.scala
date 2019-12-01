package org.github.vsuharnikov.wavesexchange

import cats.kernel.{Group, Monoid}
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import org.github.vsuharnikov.wavesexchange.domain.{AssetId, AssetPair, ClientsPortfolio, Order, OrderBook}
import org.github.vsuharnikov.wavesexchange.io._
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv
import zio.console._
import zio.nio.file.{Files, Path}
import zio.stm.{STM, TMap, TRef}
import zio.{App, Task, ZIO}

import scala.reflect.ClassTag

object MainApp extends App {
  override def run(args: List[String]) = extractArguments(args).flatMap(logic).tapError(e => putStrLn(e.getMessage)).fold(_ => 1, _ => 0)

  private def extractArguments(args: List[String]): Task[Arguments] =
    if (args.isEmpty) Task.fail(new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))
    else Task(Arguments(Path(args.head)))

  type OrderBookQueues = TMap[AssetPair, Order]

  private def logic(args: Arguments) =
    for {
      initialClientsPortfolio <- Files.readAllLines(args.outputDir / "clients.txt").flatMap { xs =>
        ZIO.fromEither(Csv.clients(xs, assets)).absorbWith(new ParseException(_))
      }
      portfoliosRef <- STM.atomically(TRef.make(initialClientsPortfolio))
      currObsRef <- STM.atomically(TRef.make(Map.empty[AssetPair, OrderBook]))
      _ <- Files
        .lines((args.outputDir / "orders.txt").toFile)
        .map(Csv.order)
        .collect { case Right(x) => x }
        .foreach { order =>
          portfoliosRef.get.flatMap { portfolios =>
            validate(order, portfolios(order.client)).fold(
              _ => STM.unit,
              _ =>
                currObsRef.get.flatMap { currObs =>
                  val (updatedAllPort, updatedOb) = append(currObs.getOrElse(order.pair, OrderBook.empty), order, portfolios)
                  portfoliosRef.set(updatedAllPort) *> currObsRef.set(currObs.updated(order.pair, updatedOb))
              }
            )
          }.commit
        }
      _ <- portfoliosRef.get.zip(currObsRef.get).commit.flatMap(Function.tupled(printResults(initialClientsPortfolio, _, _)))
    } yield ()

  private case class Arguments(outputDir: Path)

  // https://github.com/estatico/scala-newtype/issues/10
  private val assets = List('$', 'A', 'B', 'C', 'D').map(AssetId(_)).toArray(implicitly[ClassTag[Char]].asInstanceOf[ClassTag[AssetId]])

  private case class InvalidOrder(index: Int, order: Order, reason: String)

  private def printResults(initialClientsPortfolio: ClientsPortfolio,
                           updatedClientsPortfolio: ClientsPortfolio,
                           updatedObs: Map[AssetPair, OrderBook]) = {
    val obPortfolio = Group[ClientsPortfolio].inverse(Monoid.combineAll(updatedObs.values.map(_.clientsPortfolio)))
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
    ) *> putStrLn(s"Final order books:\n${updatedObs.mkString("\n")}")
  }
}

class ParseException(message: String) extends RuntimeException(message)
