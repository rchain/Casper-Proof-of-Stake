package coop.rchain.caspersimulation.network

import coop.rchain.caspersimulation.Actor
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol.Message

import scala.util.Random

case object Synchronous extends Network {
  private val shuffler: Random = new Random()

  private def actors: Iterator[Actor] = users.iterator ++ validators.iterator

  override protected def resetMessages(): Unit = Unit //nothing to reset

  //messages are sent instantly on a synchronous network
  override def sendToValidators(m: Message): Unit = validators.foreach(_.newMessage(m))

  override def timeStep()(implicit idf: IdFactory): Unit = {
    val turnOrder = shuffler.shuffle(actors) //randomize turn order for fairness

    turnOrder.foreach(_.takeTurn())
  }
}
