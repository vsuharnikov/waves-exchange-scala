package org.github.vsuharnikov.wavesexchange.source

import cats.implicits._
import cats.syntax.apply.catsSyntaxTuple3Semigroupal
import cats.syntax.traverse.toTraverseOps
import cats.{Order => _}
import org.github.vsuharnikov.wavesexchange.domain._
import org.github.vsuharnikov.wavesexchange.serde.PrimitivesParser._

import scala.collection.immutable.Queue

object Csv {
  def clients(lines: List[String], assets: List[AssetId]): ParseResult[ClientsPortfolio] =
    lines.map(client(_, assets)).sequence.map(_.combineAll) // TODO

  private def client(line: String, assets: List[AssetId]): ParseResult[ClientsPortfolio] = {
    val columns = line.split('\t').toList
    val expectedColumns = 1 + assets.length
    if (columns.length != expectedColumns)
      Left(s"Expected $expectedColumns columns, but have ${columns.length} in: $line")
    else
      columns.tail.map(int).sequence.map(xs => ClientsPortfolio(ClientId(columns.head) -> toPortfolio(assets, xs)))
  }

  private def toPortfolio(assets: List[AssetId], amounts: List[AssetAmount]): Portfolio =
    assets.zip(amounts).map(Portfolio.apply).combineAll

  def orders(csv: List[String]): ParseResult[Queue[Order]] = csv.map(order).sequence.map(_.to[Queue])

  private def order(line: String): ParseResult[Order] = {
    val parts = line.split('\t')
    if (parts.length != 5)
      Left(
        s"Expected 5 parts - <clientName, operation, assetId, pricePerOne, amount>, but there are ${parts.length}: $line")
    else {
      val Array(clientName, rawOrderType, rawAssetId, rawPrice, rawAmount) = parts
      (operation(rawOrderType), int(rawPrice), int(rawAmount))
        .mapN(Order(ClientId(clientName), _, AssetPair(AssetId(rawAssetId), Dollar), _, _))
    }
  }

  private def operation(raw: String): ParseResult[OrderType] = raw match {
    case "b" => OrderType.Bid.asRight
    case "s" => OrderType.Ask.asRight
    case x   => Left(s"Can't parse '$x' as operation")
  }
}
