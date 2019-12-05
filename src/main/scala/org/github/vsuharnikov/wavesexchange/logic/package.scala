package org.github.vsuharnikov.wavesexchange

import cats.kernel.{Group, Monoid}
import cats.syntax.group._
import org.github.vsuharnikov.wavesexchange.collections._
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.collections.groupForMap

import scala.annotation.tailrec
import scala.collection.immutable.Queue

package object logic {
  val rightUnit = Right(())

  def validate(order: Order, balance: Portfolio): Either[String, Order] = {
    val finalBalance = balance |+| order.spend
    val negativeAmount = finalBalance.p.filter { case (_, amount) => amount < 0 }.zipMap(balance.p)
    if (negativeAmount.isEmpty) Right(order)
    else {
      val stats = negativeAmount
        .map { case (assetId, (willBe, was)) => s"$was -> $willBe $assetId" }
        .mkString(", ")
      Left(s"Leads to negative amounts: $stats")
    }
  }

  def validateBalances(orderRequirement: Portfolio, balance: Portfolio): Either[String, Unit] = {
    val finalBalance = balance |+| orderRequirement
    val negativeAmount = finalBalance.p.filter { case (_, amount) => amount < 0 }.zipMap(balance.p)
    if (negativeAmount.isEmpty) rightUnit
    else {
      val stats = negativeAmount
        .map { case (assetId, (willBe, was)) => s"$was -> $willBe $assetId" }
        .mkString(", ")
      Left(s"Leads to negative amounts: $stats")
    }
  }

  @tailrec def append(orderBook: OrderBook, submitted: Order, balances: Map[ClientId, Portfolio] = Map.empty): (OrderBook, Map[ClientId, Portfolio]) =
    orderBook.best(submitted.tpe.opposite) match {
      case Some(counter) if overlaps(counter, submitted) =>
        val restCounterAmount = counter.amount - submitted.amount
        val updatedBalances = Monoid.combine(balances, execute(counter, submitted))(groupForMap)

        if (restCounterAmount == 0) (orderBook.removeBest(counter.tpe), updatedBalances)
        else if (restCounterAmount > 0)
          (orderBook.replaceBestBy(counter.copy(amount = restCounterAmount)), updatedBalances)
        else
          append(orderBook.removeBest(counter.tpe), submitted.copy(amount = -restCounterAmount), updatedBalances)

      case _ => (orderBook.append(submitted), Monoid.combine(balances, submitted.clientSpend.p)(groupForMap[ClientId, Portfolio]))
    }

  private def execute(counter: Order, submitted: Order): Map[ClientId, Portfolio] = {
    val executedPricePerOne = counter.pricePerOne
    val executedAmount = Math.min(counter.amount, submitted.amount)
    val counterReceive = counter.receive(executedPricePerOne, executedAmount)
    val submittedReceive = submitted.receive(executedPricePerOne, executedAmount)
    Map(
      counter.client -> counterReceive,
      submitted.client -> submittedReceive.remove(counterReceive),
    )
  }

  private def overlaps(counter: Order, submitted: Order): Boolean =
    submitted.tpe.askBid(submitted.pricePerOne <= counter.pricePerOne, submitted.pricePerOne >= counter.pricePerOne)

  def countAssets(allPortfolio: Map[ClientId, Portfolio], orderBooks: Map[AssetPair, OrderBook]): Portfolio = {
    val obPortfolios = Monoid.combineAll(orderBooks.values.map(_.clientsPortfolio))
    Monoid.combineAll(Monoid.combine(allPortfolio, Group[ClientsPortfolio].inverse(obPortfolios).p)(groupForMap).values)
  }

  def countAssets(allPortfolio: Map[ClientId, Portfolio], orderBook: OrderBook): Portfolio =
    Monoid.combineAll(Monoid.combine(allPortfolio, Group[ClientsPortfolio].inverse(orderBook.clientsPortfolio).p)(groupForMap).values)
}
