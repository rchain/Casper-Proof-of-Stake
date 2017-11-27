package coop.rchain.caspersimulation.network

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol.Message

case class DelayedMessage(msg: Message,
                          recipient: Validator,
                          timeUntilArrival: Int) extends TimeDependent[Option[DelayedMessage]] {
  override def timeStep()(implicit idf: IdFactory): Option[DelayedMessage] = {
    val newTime = timeUntilArrival - 1

    newTime match {
      case 0 =>
        recipient.newMessage(msg)
        None

      case _ => Some(DelayedMessage(msg, recipient, newTime))
    }
  }
}
