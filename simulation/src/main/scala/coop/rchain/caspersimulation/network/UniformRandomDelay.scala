package coop.rchain.caspersimulation.network
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol.Message

import scala.collection.mutable
import scala.util.Random

case class UniformRandomDelay(maxDelay: Int, seed: Option[Int] = None) extends Network {
  private[this] val delayGenerator: Random = seed
    .map(s => new Random(s))
    .getOrElse(new Random())

  var msgBuffer: mutable.HashSet[DelayedMessage] = mutable.HashSet.empty[DelayedMessage]

  override def send(m: Message): Unit = validators.foreach(v => {
    val delayedMessage = DelayedMessage(m, v, delayGenerator.nextInt(maxDelay) + 1)
    msgBuffer.add(delayedMessage)
  })

  override def timeStep()(implicit idf: IdFactory): Unit = {
    msgBuffer = msgBuffer.flatMap(_.timeStep())
    users.foreach(_.takeTurn())
    validators.foreach(_.takeTurn())
  }
}
