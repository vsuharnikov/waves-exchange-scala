package org.github.vsuharnikov.wavesexchange

import cats.Group
import cats.instances.map.catsKernelStdMonoidForMap

package object collections {
  final implicit class MapOps[K, V](val self: Map[K, V]) extends AnyVal {
    def zipMap(others: Map[K, V]): Map[K, (V, V)] = self.collect {
      case (k, v1) if others.contains(k) => k -> (v1, others(k))
    }
  }

  def groupForMap[K, V](implicit vGroup: Group[V]): Group[Map[K, V]] = new Group[Map[K, V]] {
    private val monoid = catsKernelStdMonoidForMap[K, V]
    override def inverse(a: Map[K, V]): Map[K, V] = a.map { case (k, v) => k -> vGroup.inverse(v) }
    override val empty: Map[K, V] = monoid.empty
    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = monoid.combine(x, y)
  }
}
