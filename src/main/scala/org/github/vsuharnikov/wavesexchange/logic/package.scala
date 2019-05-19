package org.github.vsuharnikov.wavesexchange

import cats.kernel.Monoid
import cats.syntax.semigroup.catsSyntaxSemigroup
import org.github.vsuharnikov.wavesexchange.collections.MapOps
import org.github.vsuharnikov.wavesexchange.domain._

import scala.annotation.tailrec

package object logic {
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

  def append(orderBook: OrderBook, submitted: Order): (OrderBook, ClientsPortfolio) =
    loop(orderBook, submitted, Monoid[ClientsPortfolio].empty)

  @tailrec private def loop(orderBook: OrderBook,
                            submitted: Order,
                            balanceChanges: ClientsPortfolio): (OrderBook, ClientsPortfolio) =
    orderBook.best(submitted.tpe.opposite) match {
      case Some(counter) if overlaps(counter, submitted) =>
        val restCounterAmount = counter.amount - submitted.amount
        val updatedBalanceChanges = balanceChanges |+| receive(counter, submitted)

        if (restCounterAmount == 0) (orderBook.removeBest(counter.tpe), updatedBalanceChanges)
        else if (restCounterAmount > 0)
          (orderBook.replaceBestBy(counter.copy(amount = restCounterAmount)), updatedBalanceChanges)
        else
          loop(orderBook.removeBest(counter.tpe), submitted.copy(amount = -restCounterAmount), updatedBalanceChanges)

      case _ => (orderBook.append(submitted), balanceChanges)
    }

  private def receive(counter: Order, submitted: Order): ClientsPortfolio = {
    val executedPricePerOne = counter.pricePerOne
    val executedAmount = Math.min(counter.amount, submitted.amount)
    ClientsPortfolio(counter.client -> counter.receive(executedPricePerOne, executedAmount)) |+|
      ClientsPortfolio(submitted.client -> submitted.receive(executedPricePerOne, executedAmount))
  }

  private def overlaps(counter: Order, submitted: Order): Boolean =
    if (submitted.tpe == OrderType.Ask) submitted.pricePerOne <= counter.pricePerOne
    else submitted.pricePerOne >= counter.pricePerOne
}
