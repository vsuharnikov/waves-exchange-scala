package org.github.vsuharnikov.wavesexchange

import cats.kernel.Monoid
import cats.syntax.group.catsSyntaxGroup
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.show.showInterpolator
import org.github.vsuharnikov.wavesexchange.collections.groupForMap
import org.github.vsuharnikov.wavesexchange.domain.{AssetId, AssetPair, ClientId, ClientsPortfolio, Dollar, LimitOrder, Order, OrderBook, Portfolio}
import org.github.vsuharnikov.wavesexchange.io._
import org.github.vsuharnikov.wavesexchange.logic._
import org.github.vsuharnikov.wavesexchange.source.Csv
import zio.console._
import zio.nio.file.{Files, Path}
import zio.stm.{STM, TMap, TQueue, TSemaphore}
import zio.stream.ZStream
import zio.{App, Task, ZIO}

import scala.reflect.ClassTag

sealed trait Command {
  def pair: AssetPair
}
object Command {
  case class PlaceOrder(order: Order) extends Command {
    override def pair: AssetPair = order.pair
  }
  case class Stop(pair: AssetPair) extends Command
}

object MainApp extends App {
  val orderBooksNumber = 4

  override def run(args: List[String]) = extractArguments(args).flatMap(logic).tapError(e => putStrLn(e.getMessage)).fold(_ => 1, _ => 0)

  private def extractArguments(args: List[String]): Task[Arguments] =
    if (args.isEmpty) Task.fail(new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))
    else Task(Arguments(Path(args.head)))

  private def logic(args: Arguments) =
    for {
      initialClientsPortfolio <- Files.readAllLines(args.outputDir / "clients.txt").flatMap { xs =>
        ZIO.fromEither(Csv.clients(xs, assets)).absorbWith(new ParseException(_))
      }
      wait <- STM.atomically(TSemaphore.make(0))
      availableRef <- STM.atomically(TMap.fromIterable(initialClientsPortfolio))
      reservedRef <- STM.atomically(TMap.empty[ClientId, Portfolio])
      orderBooksRef <- STM.atomically(TMap.empty[AssetPair, OrderBook])
      consumerQueue <- STM.atomically(TQueue.make[Command](10000))

      // consumer
      _ <- {
        def process(command: Command) = command match {
          case Command.PlaceOrder(order) => processOrder(availableRef, reservedRef, orderBooksRef)(order).ignore
          case Command.Stop(pair)        => wait.release.commit
        }

        def loop: ZIO[Any, Nothing, Nothing] =
          ZStream
            .fromEffect(consumerQueue.take.commit) // takeUpTo
            .groupByKey(_.pair) { (_, pairOrders) =>
              pairOrders.mapM(process)
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
          .take(11)
          .zipWithIndex
          .map {
            case (line, i) => Csv.order(i, line)
          }
          .collect { case Right(x) => x }
//          .groupByKey(_.pair) { (pair, pairOrders) =>
//            pairOrders.mapM(validateAndReserve).concat(ZStream.fromEffect(consumerQueue.offer(Command.Stop(pair)).commit)).drain
//          }
          .mapM(validateAndReserve)
          .concat {
            ZStream.fromEffect(
              consumerQueue
                .offerAll(
                  List(
                    AssetPair(AssetId('A'), Dollar),
                    AssetPair(AssetId('B'), Dollar),
                    AssetPair(AssetId('C'), Dollar),
                    AssetPair(AssetId('D'), Dollar),
                  ).map(Command.Stop)
                )
                .commit)
          }
          .runDrain
          .fork
      }
      _ <- wait.available.flatMap(x => STM.check(x >= orderBooksNumber)).commit
      _ <- {
        availableRef.fold(Map.empty[ClientId, Portfolio]) { case (r, x)  => Monoid.combine(r, Map(x))(groupForMap) } <*>
          reservedRef.fold(Map.empty[ClientId, Portfolio]) { case (r, x) => Monoid.combine(r, Map(x))(groupForMap) } <*>
          orderBooksRef.values
      }.commit.flatMap {
        case ((available, reserved), orderBooks) =>
          printResults(initialClientsPortfolio, available /* |+| reserved*/, orderBooks)
      }
    } yield ()

  private def validateAndReserveOrder(availableRef: TMap[ClientId, Portfolio], reservedRef: TMap[ClientId, Portfolio], sink: TQueue[Command])(
      order: Order) = {
    val requirements = order.spend
    putStrLn(s"$order").flatMap { _ =>
      {
        for {
          available <- availableRef.getOrElse(order.client, Portfolio.group.empty)
          reserved <- reservedRef.getOrElse(order.client, Portfolio.group.empty)
          _ <- STM.fromEither(validateBalances(available |-| reserved, requirements))
          _ <- sink.offer(Command.PlaceOrder(order))
          _ <- reservedRef.put(order.client, reserved |+| requirements)
        } yield ()
      }.commit
    }
  }

  private def processOrder(availableRef: TMap[ClientId, Portfolio],
                           reservedRef: TMap[ClientId, Portfolio],
                           orderBooksRef: TMap[AssetPair, OrderBook])(order: Order) =
    orderBooksRef
      .getOrElse(order.pair, OrderBook.empty)
      .flatMap { orderBook =>
        val (updatedOrderBook, events) = append(orderBook, LimitOrder(order))
        val (availableDiff, reservedDiff) = foldEvents(events)
        orderBooksRef.put(order.pair, updatedOrderBook) <*>
          availableDiff.foldLeft(STM.succeed(())) { case (r, (clientId, p)) => (r <*> availableRef.merge(clientId, p)(Monoid.combine)).ignore } <*>
          reservedDiff.foldLeft(STM.succeed(())) { case (r, (clientId, p))  => (r <*> reservedRef.merge(clientId, p)(Monoid.combine)).ignore }
      }
      .commit

  private case class Arguments(outputDir: Path)

  // https://github.com/estatico/scala-newtype/issues/10
  private val assets = List('$', 'A', 'B', 'C', 'D').map(AssetId(_)).toArray(implicitly[ClassTag[Char]].asInstanceOf[ClassTag[AssetId]])

  private case class InvalidOrder(index: Int, order: Order, reason: String)

  private def printResults(initialClientsPortfolio: Map[ClientId, Portfolio],
                           finalClientsPortfolio: Map[ClientId, Portfolio],
                           updatedObs: List[OrderBook]) = {
    val obPortfolio = Monoid.combineAll(updatedObs.map(_.clientsPortfolio))

    putStrLn(
      show"""Assets before:
              |${countAssets(initialClientsPortfolio)}
              |Assets after:
              |${countAssets(finalClientsPortfolio)}
              |Clients portfolio before:
              |${ClientsPortfolio(initialClientsPortfolio)}
              |Clients portfolio after:
              |${ClientsPortfolio(finalClientsPortfolio)}""".stripMargin
    ) *> putStrLn(s"Final order books:\n${updatedObs.mkString("\n")}")
  }
}

class ParseException(message: String) extends RuntimeException(message)
