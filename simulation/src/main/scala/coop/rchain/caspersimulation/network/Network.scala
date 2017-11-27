package coop.rchain.caspersimulation.network

import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.{User, Validator}
import coop.rchain.caspersimulation.protocol.Message
import coop.rchain.caspersimulation.strategy.Strategy

import scala.collection.mutable

abstract class Network extends TimeDependent[Unit] {
  val users: mutable.HashSet[User] = mutable.HashSet.empty[User]
  val validators: mutable.HashSet[Validator] = mutable.HashSet.empty[Validator]

  def createUser(implicit idf: IdFactory): Unit = users.add(User(this))
  def addUser(user: User): Unit = users.add(user)
  def createValidator(strategy: Strategy)(implicit idf: IdFactory): Unit = validators.add(Validator(strategy, this))
  def addValidator(validator: Validator): Unit = validators.add(validator)

  def send(m: Message): Unit
}
