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
import zio.blocking.Blocking
import cats.syntax.option._
import zio.console._
import zio.nio.file.{Files, Path}
import zio.stm.{STM, TMap, TQueue, TSemaphore}
import zio.stream.ZStream
import zio.{App, Task, URIO, ZIO}

import scala.reflect.ClassTag

sealed trait Command
object Command {
  case class PlaceOrder(order: Order) extends Command
  case class Stop(clientId: ClientId) extends Command
}

object MainApp extends App {
  val allAssetPairs = ('A' to 'D').toList.map(x => AssetPair(AssetId(x), Dollar))
  val orderBooksNumber = allAssetPairs.size

  override def run(args: List[String]) = extractArguments(args).flatMap(logic).tapError(e => putStrLn(e.getMessage)).fold(_ => 1, _ => 0)

  private def extractArguments(args: List[String]): Task[Arguments] =
    if (args.isEmpty) Task.fail(new IllegalArgumentException("Specify a directory with clients.txt and orders.txt"))
    else Task(Arguments(Path(args.head)))

  // TODO solve IDEA issues
  private def logic(args: Arguments): ZIO[Console with Blocking, Throwable, Unit] =
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
        def process(order: Order) = processOrder(availableRef, reservedRef, orderBooksRef)(order).ignore

        // TODO: Replace by ZStream.fromPull
        def loop: ZIO[Any, Nothing, Nothing] =
          ZStream
            .fromEffect(consumerQueue.take.commit) // takeUpTo
            .collectM {
              case Command.PlaceOrder(order) => ZIO.succeed(order.some)
              case _: Command.Stop           => wait.release.commit.map(_ => None)
            }
            .collect { case Some(x) => x }
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
        val orders = Files
          .lines((args.outputDir / "orders.txt").toFile)
          .zipWithIndex
          .map {
            case (line, i) => Csv.order(i, line)
          }
          .collect { case Right(x) => x }

        val validated = validatePar(orders, consumerQueue, validateAndReserve)
//        val validated = validateSeq(orders, allAssetPairs, consumerQueue, validateAndReserve)

        validated.runDrain.fork
      }
      _ <- wait.available.flatMap(x => STM.check(x >= initialClientsPortfolio.size)).commit
      _ <- {
        availableRef.fold(Map.empty[ClientId, Portfolio]) { case (r, x)  => Monoid.combine(r, Map(x))(groupForMap) } <*>
          reservedRef.fold(Map.empty[ClientId, Portfolio]) { case (r, x) => Monoid.combine(r, Map(x))(groupForMap) } <*>
          orderBooksRef.values
      }.commit.flatMap {
        case ((available, reserved), orderBooks) =>
          printResults(initialClientsPortfolio, available, reserved, orderBooks)
      }
    } yield ()

  private def validatePar(xs: ZStream[Any, Throwable, Order],
                          consumerQueue: TQueue[Command],
                          validateAndReserve: Order => URIO[Any, Unit]): ZStream[Any, Throwable, Unit] =
    xs.groupByKey(_.client) { (clientId, pairOrders) =>
      pairOrders.mapM(validateAndReserve).concat(ZStream.fromEffect(consumerQueue.offer(Command.Stop(clientId)).commit)).drain
    }

  private def validateSeq(xs: ZStream[Any, Throwable, Order],
                          allClientIds: List[ClientId],
                          consumerQueue: TQueue[Command],
                          validateAndReserve: Order => URIO[Any, Unit]): ZStream[Any, Throwable, Unit] =
    xs.mapM(validateAndReserve)
      .concat {
        ZStream.fromEffect(consumerQueue.offerAll(allClientIds.map(Command.Stop)).commit)
      }

  private def validateAndReserveOrder(availableRef: TMap[ClientId, Portfolio], reservedRef: TMap[ClientId, Portfolio], sink: TQueue[Command])(
      order: Order) = {
    val requirements = order.spend
    for {
      available <- availableRef.getOrElse(order.client, Portfolio.group.empty)
      reserved <- reservedRef.getOrElse(order.client, Portfolio.group.empty)
      _ <- STM.fromEither(validateBalances(available |-| reserved, requirements))
      _ <- sink.offer(Command.PlaceOrder(order))
      _ <- reservedRef.put(order.client, reserved |+| requirements)
    } yield ()
  }.commit

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
                           reserved: Map[ClientId, Portfolio],
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
              |${ClientsPortfolio(finalClientsPortfolio)}
              |obPortfolio:
              |$obPortfolio""".stripMargin
    ) *> putStrLn(s"""Final order books:
         |${updatedObs.mkString("\n")}
         |reserved:
         |${reserved.mkString("\n")}
         |""".stripMargin)
  }
}

class ParseException(message: String) extends RuntimeException(message)
