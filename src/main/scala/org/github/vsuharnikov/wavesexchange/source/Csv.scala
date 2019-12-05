package org.github.vsuharnikov.wavesexchange.source

import alleycats.std.iterable.alleycatsStdIterableFoldable
import cats.instances.either.catsStdInstancesForEither
import cats.instances.list.catsStdInstancesForList
import cats.instances.map.catsKernelStdMonoidForMap
import cats.syntax.apply.catsSyntaxTuple3Semigroupal
import cats.syntax.either._
import cats.syntax.foldable.toFoldableOps
import cats.syntax.traverse.toTraverseOps
import org.github.vsuharnikov.wavesexchange.domain._

object Csv {
  type ParseResult[T] = Either[String, T]

  def clients(lines: Iterable[String], assets: Array[AssetId]): ParseResult[Map[ClientId, Portfolio]] =
    lines.foldMapM(client(_, assets))(catsStdInstancesForEither, catsKernelStdMonoidForMap)

  private def client(line: String, assets: Array[AssetId]): ParseResult[Map[ClientId, Portfolio]] = {
    val columns = line.split('\t')
    val expectedColumns = 1 + assets.length
    if (columns.length != expectedColumns) Left(s"Expected $expectedColumns columns, but have ${columns.length} in: $line")
    else columns.tail.map(int).toList.sequence.map(xs => Map(ClientId(columns.head) -> toPortfolio(assets, xs)))
  }

  private def toPortfolio(assets: Iterable[AssetId], amounts: Iterable[AssetAmount]): Portfolio =
    Portfolio(assets.zip(amounts).toMap)

  def order(i: Int, line: String): ParseResult[Order] =
    line.split('\t') match {
      case Array(clientName, rawOrderType, rawAssetId, rawPrice, rawAmount) =>
        if (rawAssetId.length == 1)
          (operation(rawOrderType), int(rawPrice), int(rawAmount))
            .mapN(Order(OrderId(i), ClientId(clientName), _, AssetPair(AssetId(rawAssetId.head), Dollar), _, _))
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
