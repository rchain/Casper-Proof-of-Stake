package coop.rchain.caspersimulation.onchainstate

import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}

import scala.util.Random

sealed abstract class Resource extends Identifiable {
  def isStopped: Boolean
  def isProduce: Boolean
  def isConsume: Boolean
}

object Resource {
  private val rnd = new Random()
  private val maxIndex = 15

  val producePrefix: String = "Produce-"
  val consumePrefix: String = "Consume-"

  def matches(p: Produce, c: Consume): Boolean =
    p.id.drop(producePrefix.length) == c.id.drop(consumePrefix.length)

  def random: Resource = {
    val isProduction = rnd.nextBoolean()
    val index = rnd.nextInt(maxIndex).formatted("%04d")
    if (isProduction) {
      val continue = rnd.nextBoolean()
      if (continue) {
        Produce(producePrefix + index, random)
      } else {
        Produce(producePrefix + index, Stopped)
      }
    } else {
      Consume(consumePrefix + index)
    }
  }
}

case object Stopped extends Resource {
  override val id: String = "Stopped"

  override def isStopped: Boolean = true
  override def isProduce: Boolean = false
  override def isConsume: Boolean = false
}

case class Produce(id: String, continuation: Resource = Stopped) extends Resource {
  override def isStopped: Boolean = false
  override def isProduce: Boolean = true
  override def isConsume: Boolean = false
}

object Produce {
  def apply(continuation: Resource)(implicit idf: IdFactory): Produce =
    Produce(idf.next(Resource.producePrefix), continuation)
}

case class Consume(id: String) extends Resource {
  override def isStopped: Boolean = false
  override def isProduce: Boolean = false
  override def isConsume: Boolean = true
}

object Consume {
  def apply()(implicit idf: IdFactory): Consume = Consume(idf.next(Resource.consumePrefix))
}
