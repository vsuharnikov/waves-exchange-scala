package org.github.vsuharnikov.wavesexchange

package object collections {
  final implicit class MapOps[K, V](val self: Map[K, V]) extends AnyVal {
    def zipMap(others: Map[K, V]): Map[K, (V, V)] = self.collect {
      case (k, v1) if others.contains(k) => k -> (v1, others(k))
    }
  }
}
