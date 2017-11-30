package coop.rchain.caspersimulation.network
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol.Message

import scala.collection.mutable
import scala.util.Random

/**
  * A model of an asynchronous network in which all messages are delayed from arriving to
  * each validator by a random amount (this amount is different for each validator). This
  * amount is a uniformly distributed integer on [0, maxDelay] and corresponds to the number
  * of actions the recipient will take before receiving the message.
  * @param maxDelay maximum number of actions recipient will take before receiving the message
  * @param seed (optional) seed used for the random number generator used in assigning delays
  */
case class UniformRandomDelay(maxDelay: Int, seed: Option[Int] = None) extends Network {
  private[this] val delayGenerator: Random = seed
    .map(s => new Random(s))
    .getOrElse(new Random())

  var msgBuffer: mutable.HashSet[DelayedMessage] = mutable.HashSet.empty[DelayedMessage]

  override protected def resetMessages(): Unit = msgBuffer.clear()

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
