package org.github.vsuharnikov.wavesexchange

import cats.instances.tuple.catsKernelStdGroupForTuple2
import cats.kernel.{Group, Monoid}
import cats.syntax.group.{catsSyntaxGroup, catsSyntaxSemigroup}
import org.github.vsuharnikov.wavesexchange.collections._
import org.github.vsuharnikov.wavesexchange.domain._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

package object logic {
  implicit val clientPorfoliosGroup = groupForMap[ClientId, Portfolio]

  val rightUnit = Right(())

  def validateBalances(balance: Portfolio, orderRequirement: Portfolio): Either[String, Unit] = {
    val finalBalance = balance |-| orderRequirement
    val negativeAmount = finalBalance.p.filter { case (_, amount) => amount < 0 }.zipMap(balance.p)
    if (negativeAmount.isEmpty) rightUnit
    else {
      val stats = negativeAmount
        .map { case (assetId, (willBe, was)) => s"$was -> $willBe $assetId" }
        .mkString(", ")
      Left(s"Leads to negative amounts: $stats")
    }
  }

  def append(orderBook: OrderBook, submitted: LimitOrder): (OrderBook, Queue[OrderEvent]) =
    append(orderBook, submitted, Queue(OrderEvent.New(submitted)))

  @tailrec private def append(orderBook: OrderBook, submitted: LimitOrder, events: Queue[OrderEvent]): (OrderBook, Queue[OrderEvent]) =
    orderBook.best(submitted.order.tpe.opposite) match {
      case Some(counter) if overlaps(counter.order, submitted.order) =>
        val updatedRestCounterAmount = counter.restAmount - submitted.restAmount
        val updatedEvents = events.enqueue(OrderEvent.Executed(counter, submitted))

        if (updatedRestCounterAmount == 0) (orderBook.removeBest(counter.order.tpe), updatedEvents)
        else if (updatedRestCounterAmount > 0) (orderBook.replaceBestBy(counter.copy(restAmount = updatedRestCounterAmount)), updatedEvents)
        else append(orderBook.removeBest(counter.order.tpe), submitted.copy(restAmount = -updatedRestCounterAmount), updatedEvents)

      case _ => (orderBook.append(submitted), events.enqueue(OrderEvent.Placed(submitted)))
    }

  def foldEvents(events: Queue[OrderEvent],
                 aDiff: Map[ClientId, Portfolio] = Map.empty,
                 rDiff: Map[ClientId, Portfolio] = Map.empty): (Map[ClientId, Portfolio], Map[ClientId, Portfolio]) =
    events.foldLeft((aDiff, rDiff)) {
      case (r, evt) =>
        evt match {
          // TODO because we match with counter price, we need unreserve more! But it should work ... we have reserved diff
          case OrderEvent.Executed(maker, taker) => r |+| execute(maker, taker)
          case _                                 => r
        }
    }

  def execute(counter: LimitOrder, submitted: LimitOrder): (Map[ClientId, Portfolio], Map[ClientId, Portfolio]) = {
    // TODO same orders?
    // TODO wrong price?

    val executedPricePerOne = counter.order.pricePerOne
    val executedAmount = Math.min(counter.restAmount, submitted.restAmount)
    val counterReceive = counter.order.receive(executedPricePerOne, executedAmount)
    val submittedReceive = submitted.order.receive(executedPricePerOne, executedAmount)

    val counterDiff = counterReceive |-| submittedReceive

    val aDiff = Map(
      counter.order.client -> counterDiff,
      submitted.order.client -> Group.inverse(counterDiff)
    )

    val rDiff = Group.inverse(
      Map(counter.order.client -> counter.copy(restAmount = executedAmount).spend) |+|
        Map(submitted.order.client -> submitted.copy(restAmount = executedAmount).spend)
    )

    (aDiff, rDiff)
  }

  private def overlaps(counter: Order, submitted: Order): Boolean =
    submitted.tpe.askBid(submitted.pricePerOne <= counter.pricePerOne, submitted.pricePerOne >= counter.pricePerOne)

  def countAssets(allPortfolio: Map[ClientId, Portfolio], orderBooks: Map[AssetPair, OrderBook]): Portfolio = {
    val obPortfolios = Monoid.combineAll(orderBooks.values.map(_.clientsPortfolio))
    Monoid.combineAll(Monoid.combine(allPortfolio, Group[ClientsPortfolio].inverse(obPortfolios).p)(groupForMap).values)
  }

  def countAssets(allPortfolio: Map[ClientId, Portfolio], orderBook: OrderBook): Portfolio =
    Monoid.combineAll(Monoid.combine(allPortfolio, Group[ClientsPortfolio].inverse(orderBook.clientsPortfolio).p)(groupForMap).values)

  def countAssets(allPortfolio: Map[ClientId, Portfolio]): Portfolio = Monoid.combineAll(allPortfolio.values)
}
