package coop.rchain.caspersimulation

import coop.rchain.caspersimulation.block.{Block, DagUtil, Transactions}
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.protocol._
import coop.rchain.caspersimulation.strategy.Strategy

sealed abstract class Actor extends Identifiable {
  def takeTurn()(implicit idf: IdFactory): Unit
}

case object Initializer extends Actor {
  override val id: String = "Initializer"

  override def takeTurn()(implicit idf: IdFactory): Unit = Unit //do nothing
}

case class User(id: String, network: Network) extends Actor {
  private[this] val revGenerator = new scala.util.Random()
  private [this] val sd: Double = 5d
  private [this] val mean: Double = 20d

  def requestSmartContract(rev: Double)(implicit idf: IdFactory): Unit = {
    val contract = SmartContract(rev)
    val msg = SmartContractRequest(contract, this)
    network.send(msg)
  }

  override def takeTurn()(implicit idf: IdFactory): Unit = {
    val makeContract = true//revGenerator.nextBoolean()

    if (makeContract) {
      requestSmartContract(revGenerator.nextGaussian() * sd + mean)
    }
  }
}

object User {
  def apply(network: Network)(implicit idf: IdFactory): User = new User(idf.next("user_"), network)
}

case class Validator(id: String, strategy: Strategy, network: Network, var state: State) extends Actor {
  def pickContracts: IndexedSeq[SmartContract] = strategy.pickContracts(this)

  def assignPC(blockData: Either[IndexedSeq[SmartContract], IndexedSeq[Block]]): PoliticalCapital = {
    strategy.assignPC(state.pc, blockData)
  }

  def pickAckBlocks: IndexedSeq[Block] = strategy.pickAckBlocks(this)

  def newMessage(msg: Message): Unit = {
    state = state.receive(msg)
  }

  override def takeTurn()(implicit idf: IdFactory): Unit = {
    state = strategy.stateUpdate(this)
  }

  final def revEarned: Double = {
    val dagHead = Ghost.forkChoice(state.blockHist)

    DagUtil.foldDag(0d, dagHead, (r: Double, b: Block) => b match {
      case t: Transactions if t.creator == this => r + t.txns.iterator.map(_.rev).sum
      case _ => r
    })
  }
}

object Validator {
  def apply(strategy: Strategy, network: Network)(implicit idf: IdFactory): Validator =
    new Validator(idf.next("validator_"), strategy, network, State.initialize)
}