package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, Genesis}
import coop.rchain.caspersimulation._

sealed abstract class Message {
  val sender: Actor
}

case class SmartContractRequest(contract: SmartContract, sender: User) extends Message

case class BlockCreation(block: Block) extends Message {
  override val sender: Actor = block.creator
}

object BlockCreation {
  def genesis: BlockCreation = BlockCreation(Genesis)
}