package org.github.vsuharnikov.wavesexchange

import java.io.{File, FileInputStream}
import java.nio.charset.{Charset, StandardCharsets}

import zio.nio.file.Files
import zio.stream.ZStream
import zio.{ZIO, ZManaged}

package object io {
  final implicit class FilesOps(val self: Files.type) extends AnyVal {
    def lines(file: File, charset: Charset = StandardCharsets.UTF_8): ZStream[Any, Throwable, String] = {
      val reader = ZManaged.make(ZIO(new FileInputStream(file)))(x => ZIO(x.close()).ignore)
      ZStream.managed(reader).flatMap(x => ZStream.fromIterator { ZIO(new LinesIterator(x, charset)) })
    }
  }
}
