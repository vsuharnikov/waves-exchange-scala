package org.github.vsuharnikov.wavesexchange

import cats.kernel.{Group, Monoid}
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import cats.syntax.group.catsSyntaxGroup
import org.github.vsuharnikov.wavesexchange.collections.groupForMap
import org.github.vsuharnikov.wavesexchange.domain.{AssetId, AssetPair, ClientId, ClientsPortfolio, Order, OrderBook, Portfolio}
import org.github.vsuharnikov.wavesexchange.io._
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv
import zio.console._
import zio.nio.file.{Files, Path}
import zio.stm.{STM, TMap, TQueue, TRef}
import zio.stream.ZStream
import zio.{App, Task, ZIO}

import scala.reflect.ClassTag

object MainApp extends App {
  override def run(args: List[String]) = extractArguments(args).flatMap(logic).tapError(e => putStrLn(e.getMessage)).fold(_ => 1, _ => 0)

  private def extractArguments(args: List[String]): Task[Arguments] =
    if (args.isEmpty) Task.fail(new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))
    else Task(Arguments(Path(args.head)))

  private def logic(args: Arguments) =
    for {
      initialClientsPortfolio <- Files.readAllLines(args.outputDir / "clients.txt").flatMap { xs =>
        ZIO.fromEither(Csv.clients(xs, assets)).absorbWith(new ParseException(_))
      }
      portfoliosRef <- STM.atomically(TRef.make(initialClientsPortfolio))
      currObsRef <- STM.atomically(TRef.make(Map.empty[AssetPair, OrderBook]))
      consumerQueue <- STM.atomically(TQueue.make[Order](10000))
      // consumer
      _ <- {
        ZStream
          .fromEffect(consumerQueue.take.commit) // takeUpTo
          .groupByKey(_.pair) { (_, pairOrders) =>
            pairOrders.mapM(validateAndReserve).drain
          }
      }
      // producer
      _ <- {
        val validateAndReserve = validateAndReserveOrder(portfoliosRef, currObsRef, consumerQueue)(_)
        Files
          .lines((args.outputDir / "orders.txt").toFile)
          .map(Csv.order)
          .collect { case Right(x) => x }
          .groupByKey(_.pair) { (_, pairOrders) =>
            pairOrders.mapM(validateAndReserve).drain
          }
          .runDrain
          .fork
      }
      _ <- portfoliosRef.get.zip(currObsRef.get).commit.flatMap(Function.tupled(printResults(initialClientsPortfolio, _, _)))
    } yield ()

  private def validateAndReserveOrder(availableRef: TMap[ClientId, Portfolio], reservedRef: TMap[ClientId, Portfolio], sink: TQueue[Order])(order: Order) =
    (
      for {
        available <- availableRef.getOrElse(order.client, Portfolio.group.empty)
        reserved <- availableRef.getOrElse(order.client, Portfolio.group.empty)
        _ <- STM.fromEither(validate(order, available |-| reserved))
        _ <- sink.offer(order)
        _ <- reservedRef.put(order.client, reserved |+| order.spend)
      } yield ()
    ).commit

  private def processOrder(portfoliosRef: TRef[ClientsPortfolio], currObsRef: TRef[Map[AssetPair, OrderBook]])(order: Order) =
    portfoliosRef.get.flatMap { portfolios =>
      validate(order, portfolios(order.client)).fold(
        _ => STM.unit,
        _ =>
          currObsRef.get.flatMap { currObs =>
            val (updatedAllPort, updatedOb) = append(currObs.getOrElse(order.pair, OrderBook.empty), order, portfolios)
            portfoliosRef.set(updatedAllPort) <*> currObsRef.set(currObs.updated(order.pair, updatedOb))
        }
      )
    }.commit

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
