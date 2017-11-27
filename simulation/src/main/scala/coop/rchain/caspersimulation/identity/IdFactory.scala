package coop.rchain.caspersimulation.identity

import scala.collection.mutable

class IdFactory {
  private[this] val prefixes: mutable.HashMap[String, Iterator[Int]] = mutable.HashMap.empty[String, Iterator[Int]]

  def next(prefix: String): String = {
    val counter = prefixes.getOrElseUpdate(prefix, Iterator.from(0))
    val index = counter.next().formatted("%04d")
    prefix + index
  }
}
