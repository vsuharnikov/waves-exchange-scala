package org.github.vsuharnikov.wavesexchange.domain

object Implicits {
  final implicit class SideOps(val self: Side) extends AnyVal {
    def worst: Option[Order] = self.orders.lastOption.flatMap(_._2.lastOption)
    def minAmount: AssetAmount = self.orders.values.flatten.map(_.amount).min
  }
}
