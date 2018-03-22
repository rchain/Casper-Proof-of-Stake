package coop.rchain.caspersimulation.network

import coop.rchain.caspersimulation.block.BlockDag
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.{TimeDependent, User, Validator}
import coop.rchain.caspersimulation.protocol.{BlockCreation, Message}

import scala.collection.mutable

abstract class Network extends TimeDependent[Unit] {
  val users: mutable.HashSet[User] = mutable.HashSet.empty[User]
  val validators: mutable.HashSet[Validator] = mutable.HashSet.empty[Validator]
  val globalDag: BlockDag = new BlockDag()

  def createUser(implicit idf: IdFactory): Unit = users.add(User(this))
  def addUser(user: User): Unit = users.add(user)
  def createValidator(implicit idf: IdFactory): Unit = validators.add(Validator(this))
  def addValidator(validator: Validator): Unit = validators.add(validator)

  final def reset(): Unit = {
    users.clear()
    validators.clear()
    resetMessages()
  }

  def send(m: Message): Unit = {
    sendToValidators(m)
    m match {
      case BlockCreation(b) => globalDag.add(b)
      case _ => Unit
    }
  }

  protected def sendToValidators(m: Message): Unit
  protected def resetMessages(): Unit
}
