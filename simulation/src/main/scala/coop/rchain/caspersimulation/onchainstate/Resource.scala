package coop.rchain.caspersimulation.onchainstate

import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}

import scala.collection.immutable.HashSet
import scala.util.Random

sealed abstract class Resource extends Identifiable {}

object Resource {
  private val rnd = new Random()

  val producePrefix: String = "Produce-"
  val consumePrefix: String = "Consume-"

  def consumes(resource: HashSet[Resource]): HashSet[Consume] = toConsumesProducesPair(resource)._1
  def produces(resource: HashSet[Resource]): HashSet[Produce] = toConsumesProducesPair(resource)._2

  private def toConsumesProducesPair(resource: HashSet[Resource]): (HashSet[Consume], HashSet[Produce]) =
    resource.foldLeft(HashSet[Consume](), HashSet[Produce]()) {
      case ((cs, ps), c: Consume) => (cs + c, ps)
      case ((cs, ps), p: Produce) => (cs, ps + p)
      case ((cs, ps), _) => (cs, ps)
    }

  def matches(c: Consume, p: Produce): Boolean =
    p.id.drop(producePrefix.length) == c.id.drop(consumePrefix.length)

  def random: Resource = {
    val isProduction = rnd.nextBoolean()
    // An arbitrary exponential function that puts 50% of the transactions to the first 33 indices
    // and 99% of the transactions to the first 9766 indices.
    // This is trying to roughly model the distribution of transactions
    // across names in a production setting.
    val exponentialRandom = (Math.pow(Math.log(rnd.nextDouble()), 3) / -0.01).toInt
    val index = exponentialRandom.formatted("%04d")
    if (isProduction) {
      val continue = rnd.nextBoolean()
      if (continue) {
        Consume(producePrefix + index, random)
      } else {
        Consume(producePrefix + index, Stopped)
      }
    } else {
      Produce(consumePrefix + index)
    }
  }
}

case object Stopped extends Resource {
  override val id: String = "Stopped"
}

case class Consume(id: String, continuation: Resource = Stopped) extends Resource

object Consume {
  def apply(continuation: Resource)(implicit idf: IdFactory): Consume =
    Consume(idf.next(Resource.producePrefix), continuation)
}

case class Produce(id: String) extends Resource

object Produce {
  def apply()(implicit idf: IdFactory): Produce = Produce(idf.next(Resource.consumePrefix))
}
