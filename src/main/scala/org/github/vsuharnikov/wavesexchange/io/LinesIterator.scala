package org.github.vsuharnikov.wavesexchange.io

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.Charset
import java.util.NoSuchElementException

class LinesIterator(is: InputStream, charset: Charset) extends Iterator[String]() {
  private val reader = new BufferedReader(new InputStreamReader(is, charset))
  private var nextLine = Option.empty[String]

  override def hasNext: Boolean =
    if (nextLine.isDefined) true
    else {
      nextLine = Option(reader.readLine())
      nextLine.isDefined
    }

  override def next: String =
    if (hasNext) {
      val line: String = nextLine.get
      nextLine = None
      line
    } else throw new NoSuchElementException
}
