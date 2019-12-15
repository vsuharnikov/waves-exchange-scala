package org.github.vsuharnikov.wavesexchange

import cats.kernel.Monoid
import zio.stm.{STM, TMap}

package object stm {
  final implicit class TMapOps[K, V](val self: TMap[K, V]) extends AnyVal {
    def foldMapM[R](z: R)(f: (K, V) => R)(implicit m: Monoid[R]): STM[Nothing, R] = {
      val ft = Function.tupled(f)
      self.fold(m.empty) { case (r, x) => m.combine(r, ft(x)) }
    }

    def mergeMap(xs: Map[K, V])(implicit m: Monoid[V]): STM[Nothing, Unit] =
      xs.foldLeft(STM.unit) { case (r, x) => (r <*> self.merge(x._1, x._2)(m.combine)).ignore }
  }
}
