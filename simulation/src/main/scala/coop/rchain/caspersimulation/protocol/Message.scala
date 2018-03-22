package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.Block
import coop.rchain.caspersimulation._
import coop.rchain.caspersimulation.onchainstate.Deploy

sealed abstract class Message {
  val sender: Actor
}

case class DeployRequest(transaction: Deploy, sender: User) extends Message

case class BlockCreation(block: Block) extends Message {
  override val sender: Actor = block.creator
}