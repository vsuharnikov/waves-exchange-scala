package org.github.vsuharnikov.wavesexchange

import cats.kernel.{Group, Monoid}
import cats.syntax.group._
import org.github.vsuharnikov.wavesexchange.collections.MapOps
import org.github.vsuharnikov.wavesexchange.domain._

import scala.annotation.tailrec

package object logic {
  sealed trait OrderEvent
  object OrderEvent {
    case class Added(order: Order)
  }

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

  @tailrec def append(orderBook: OrderBook,
                      submitted: Order,
                      balances: ClientsPortfolio = Monoid[ClientsPortfolio].empty): (ClientsPortfolio, OrderBook) =
    orderBook.best(submitted.tpe.opposite) match {
      case Some(counter) if overlaps(counter, submitted) =>
        val restCounterAmount = counter.amount - submitted.amount
        val updatedBalances = balances |+| execute(counter, submitted)

        if (restCounterAmount == 0) (updatedBalances, orderBook.removeBest(counter.tpe))
        else if (restCounterAmount > 0)
          (updatedBalances, orderBook.replaceBestBy(counter.copy(amount = restCounterAmount)))
        else
          append(orderBook.removeBest(counter.tpe), submitted.copy(amount = -restCounterAmount), updatedBalances)

      case _ => (balances |+| submitted.clientSpend, orderBook.append(submitted))
    }

  private def execute(counter: Order, submitted: Order): ClientsPortfolio = {
    val executedPricePerOne = counter.pricePerOne
    val executedAmount = Math.min(counter.amount, submitted.amount)
    val counterReceive = counter.receive(executedPricePerOne, executedAmount)
    val submittedReceive = submitted.receive(executedPricePerOne, executedAmount)
    ClientsPortfolio(
      counter.client -> counterReceive,
      submitted.client -> submittedReceive.remove(counterReceive),
    )
  }

  private def overlaps(counter: Order, submitted: Order): Boolean =
    submitted.tpe.askBid(submitted.pricePerOne <= counter.pricePerOne, submitted.pricePerOne >= counter.pricePerOne)

  def countAssets(allPortfolio: ClientsPortfolio, orderBooks: Map[AssetPair, OrderBook]): Portfolio = {
    val obPortfolios = Monoid.combineAll(orderBooks.values.map(_.clientsPortfolio))
    Monoid.combineAll((allPortfolio |+| Group[ClientsPortfolio].inverse(obPortfolios)).values)
  }

  def countAssets(allPortfolio: ClientsPortfolio, orderBook: OrderBook): Portfolio =
    Monoid.combineAll((allPortfolio |+| Group[ClientsPortfolio].inverse(orderBook.clientsPortfolio)).values)
}
