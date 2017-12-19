package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.{SmartContract, Validator}
import coop.rchain.caspersimulation.block.{Acknowledgements, Block, Transactions}
import coop.rchain.caspersimulation.identity.IdFactory

case class State(pc: PoliticalCapital, msgHist: IndexedSeq[Message]) {

  def receive(msg: Message): State = State(pc, msgHist :+ msg)

  def act(validator: Validator,
          blockData: Either[IndexedSeq[SmartContract], IndexedSeq[Block]],
          pca: PoliticalCapital)(implicit idf: IdFactory): State = {
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
    State(newPCBalance + pce, msgHist :+ msg)

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
    State(new PoliticalCapital(0d), IndexedSeq(BlockCreation.genesis))
  }
}