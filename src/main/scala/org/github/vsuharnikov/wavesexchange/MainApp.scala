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
import cats.syntax.group._
import zio.stream.ZStream
import zio.{App, Task, ZIO}
import zio.duration.durationInt

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
      availableRef <- STM.atomically(TMap.fromIterable(initialClientsPortfolio))
      reservedRef <- STM.atomically(TMap.empty[ClientId, Portfolio])
      orderBooksRef <- STM.atomically(TMap.empty[AssetPair, OrderBook])
      consumerQueue <- STM.atomically(TQueue.make[Order](10000))

      // consumer
      _ <- {
        val process = processOrder(reservedRef, orderBooksRef)(_)

        def loop: ZIO[Any, Nothing, Nothing] =
          ZStream
            .fromEffect(consumerQueue.take.commit) // takeUpTo
            .groupByKey(_.pair) { (_, pairOrders) =>
              pairOrders.mapM(process).drain
            }
            .runDrain
            .flatMap(_ => loop)

        loop.fork
      }
      // producer
      _ <- {
        val validateAndReserve = validateAndReserveOrder(availableRef, reservedRef, consumerQueue)(_: Order).ignore
        Files
          .lines((args.outputDir / "orders.txt").toFile)
          .zipWithIndex
          .map {
            case (line, i) => Csv.order(i, line)
          }
          .collect { case Right(x) => x }
          .groupByKey(_.pair) { (_, pairOrders) =>
            pairOrders.mapM(validateAndReserve).drain
          }
          .runDrain
          .fork
      }
      _ <- ZIO.sleep(5.seconds)
      _ <- {
        availableRef.fold(Map.empty[ClientId, Portfolio]) { case (r, x)  => Monoid.combine(r, Map(x))(groupForMap) } <*>
          reservedRef.fold(Map.empty[ClientId, Portfolio]) { case (r, x) => Monoid.combine(r, Map(x))(groupForMap) } <*>
          orderBooksRef.values
      }.commit.flatMap {
        case ((available, reserved), orderBooks) =>
          printResults(initialClientsPortfolio, ClientsPortfolio(Monoid.combine(available, reserved)(groupForMap)), orderBooks)
      }
    } yield ()

  private def validateAndReserveOrder(availableRef: TMap[ClientId, Portfolio], reservedRef: TMap[ClientId, Portfolio], sink: TQueue[Order])(
      order: Order) = {
    val requirements = order.spend
    for {
      available <- availableRef.getOrElse(order.client, Portfolio.group.empty)
      reserved <- availableRef.getOrElse(order.client, Portfolio.group.empty)
      _ <- STM.fromEither(validateBalances(requirements, available |-| reserved))
      _ <- sink.offer(order)
      _ <- reservedRef.put(order.client, reserved |+| order.spend)
    } yield ()
  }.commit

  private def processOrder(reservedRef: TMap[ClientId, Portfolio], orderBooksRef: TMap[AssetPair, OrderBook])(order: Order) =
    orderBooksRef
      .getOrElse(order.pair, OrderBook.empty)
      .flatMap { orderBook =>
        val (updatedOrderBook, portfoliosDiff) = append(orderBook, order)
        // TODO
        orderBooksRef.put(order.pair, updatedOrderBook) <*> portfoliosDiff.foldLeft(STM.succeed(())) {
          case (r, (clientId, p)) => (r <*> reservedRef.merge(clientId, p)(Monoid.combine)).ignore
        }
      }
      .commit

  private case class Arguments(outputDir: Path)

  // https://github.com/estatico/scala-newtype/issues/10
  private val assets = List('$', 'A', 'B', 'C', 'D').map(AssetId(_)).toArray(implicitly[ClassTag[Char]].asInstanceOf[ClassTag[AssetId]])

  private case class InvalidOrder(index: Int, order: Order, reason: String)

  private def printResults(initialClientsPortfolio: Map[ClientId, Portfolio],
                           updatedClientsPortfolio: ClientsPortfolio,
                           updatedObs: List[OrderBook]) = {
    val obPortfolio = Group[ClientsPortfolio].inverse(Monoid.combineAll(updatedObs.map(_.clientsPortfolio)))
    val finalClientsPortfolio = updatedClientsPortfolio |+| obPortfolio
    val assetsAfter = Monoid.combineAll(finalClientsPortfolio.values)

    putStrLn(
      show"""Assets before:
              |${countAssets(initialClientsPortfolio, OrderBook.empty)}
              |Assets after:
              |$assetsAfter
              |Clients portfolio before:
              |${ClientsPortfolio(initialClientsPortfolio)}
              |Clients portfolio after:
              |$finalClientsPortfolio""".stripMargin
    ) *> putStrLn(s"Final order books:\n${updatedObs.mkString("\n")}")
  }
}

class ParseException(message: String) extends RuntimeException(message)
