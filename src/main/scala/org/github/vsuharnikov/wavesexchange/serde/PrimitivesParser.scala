package org.github.vsuharnikov.wavesexchange.serde

object PrimitivesParser {
  type ParseResult[T] = Either[String, T]

  def int(raw: String): ParseResult[Int] = try {
    Right(raw.toInt)
  } catch {
    case e: NumberFormatException => Left(e.getMessage)
  }
}
