package org.github.vsuharnikov.wavesexchange.source

import alleycats.std.iterable.alleycatsStdIterableFoldable
import cats.implicits._
import cats.syntax.apply.catsSyntaxTuple3Semigroupal
import cats.syntax.traverse.toTraverseOps
import cats.{Order => _}
import org.github.vsuharnikov.wavesexchange.domain._

object Csv {
  type ParseResult[T] = Either[String, T]

  def clients(lines: Iterable[String], assets: Array[AssetId]): ParseResult[ClientsPortfolio] =
    lines.foldMapM(client(_, assets))

  private def client(line: String, assets: Array[AssetId]): ParseResult[ClientsPortfolio] = {
    val columns = line.split('\t')
    val expectedColumns = 1 + assets.length
    if (columns.length != expectedColumns) Left(s"Expected $expectedColumns columns, but have ${columns.length} in: $line")
    else columns.tail.map(int).toList.sequence.map(xs => ClientsPortfolio(ClientId(columns.head) -> toPortfolio(assets, xs)))
  }

  private def toPortfolio(assets: Iterable[AssetId], amounts: Iterable[AssetAmount]): Portfolio =
    Portfolio(assets.zip(amounts).toMap)

  def order(line: String): ParseResult[Order] =
    line.split('\t') match {
      case Array(clientName, rawOrderType, rawAssetId, rawPrice, rawAmount) =>
        if (rawAssetId.length == 1)
          (operation(rawOrderType), int(rawPrice), int(rawAmount))
            .mapN(Order(ClientId(clientName), _, AssetPair(AssetId(rawAssetId.head), Dollar), _, _))
        else Left(s"Invalid AssetId: $rawAssetId, expected one symbol")
      case xs =>
        Left(s"Expected 5 parts - <clientId, operation, assetId, pricePerOne, amount>, but there are ${xs.length}: $line")
    }

  private def operation(raw: String): ParseResult[OrderType] = raw match {
    case "b" => OrderType.Bid.asRight
    case "s" => OrderType.Ask.asRight
    case x   => Left(s"Can't parse '$x' as operation")
  }

  private def int(raw: String): ParseResult[Int] =
    try Right(raw.toInt)
    catch { case e: NumberFormatException => Left(e.getMessage) }
}
