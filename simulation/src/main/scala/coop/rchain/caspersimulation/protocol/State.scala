package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.{SmartContract, Validator}
import coop.rchain.caspersimulation.block.{Acknowledgements, Block, Transactions}
import coop.rchain.caspersimulation.identity.IdFactory

import scala.collection.immutable.IndexedSeq

case class State(intendedAction: Action, pc: PoliticalCapital, msgHist: IndexedSeq[Message]) {
  def changeIntent: State = intendedAction match {
    case Propose => State(Acknowledge, pc, msgHist)
    case Acknowledge => State(Propose, pc, msgHist)
  }

  def receive(msg: Message): State = State(intendedAction, pc, msgHist :+ msg)

  def act(validator: Validator)(implicit idf: IdFactory): State = {
    val blockData = intendedAction match {
      case Propose =>
        Left(validator.pickContracts)

      case Acknowledge =>
        Right(validator.pickAckBlocks)
    }

    val pca = validator.assignPC(blockData)
    val newPCBalance = pc - pca
    assert(newPCBalance.amount >= 0)
    val observedBlocks = blockHist

    val newBlock = blockData match {
      case Left(txns) =>
        val parent = Ghost.forkChoice(observedBlocks)
        Transactions(
          validator,
          IndexedSeq(parent),
          pca,
          txns,
          observedBlocks
        )

      case Right(blocks) =>
        Acknowledgements(
          validator,
          blocks,
          pca,
          observedBlocks
        )

    }

    val pce = newBlock.pce //political capital earned
    val msg = BlockCreation(newBlock)
    validator.network.send(msg)
    State(intendedAction, newPCBalance + pce, msgHist :+ msg)
  }

  def blockHist: IndexedSeq[Block] = msgHist.flatMap{
    case b: BlockCreation => Some(b.block)
    case _ => None
  }

  def contractHist: IndexedSeq[SmartContract] = msgHist.flatMap{
    case c: SmartContractRequest => Some(c.contract)
    case _ => None
  }
}

object State {
  def initialize: State = {
    State(Acknowledge, new PoliticalCapital(0d), IndexedSeq(BlockCreation.genesis))
  }
}