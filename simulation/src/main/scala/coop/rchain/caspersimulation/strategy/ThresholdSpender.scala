package coop.rchain.caspersimulation.strategy

import coop.rchain.caspersimulation.{SmartContract, Validator}
import coop.rchain.caspersimulation.block.{Block, DagUtil}
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol._

/**
  * A strategy which will always accumulate political capital (acknowledge other blocks) when
  * their political capital balance is below some threshold, and always propose a block with all
  * available contracts and political capital attached when it is above the threshold.
  * @param threshold amount of political capital required to choose to propose a block
  */
case class ThresholdSpender(threshold: PoliticalCapital) extends Strategy {
  override def pickAckBlocks(validator: Validator): IndexedSeq[Block] = {
    val latestBlocks = DagUtil.latestBlocks(validator.state.blockHist)
    val unAck = latestBlocks.filter(b => !Strategy.previouslyAcknowledged(b, validator))
    Strategy.maximalIndependentSubset(unAck.toIndexedSeq).toIndexedSeq
  }

  override def pickContracts(validator: Validator): IndexedSeq[SmartContract] =
    Strategy.availableContracts(validator.state).toIndexedSeq

  override def assignPC(pcBalance: PoliticalCapital,
                        blockData: Either[IndexedSeq[SmartContract], IndexedSeq[Block]]): PoliticalCapital =
    blockData match {
      case Left(_) => pcBalance //spend it all when proposing
      case Right(_) => new PoliticalCapital(0d) //spend nothing when acknowledging
    }

  override def stateUpdate(validator: Validator)(implicit idf: IdFactory): State = {
    if (validator.state.pc.amount > threshold.amount & pickContracts(validator).nonEmpty) {
      val proposeState = if (validator.state.intendedAction == Acknowledge) {
        validator.state.changeIntent
      } else {
        validator.state
      }
      proposeState.act(validator)
    } else {
      if (pickAckBlocks(validator).nonEmpty) {
        val ackState = if (validator.state.intendedAction == Propose) {
          validator.state.changeIntent
        } else {
          validator.state
        }
        ackState.act(validator)
      } else {
        validator.state
      }
    }
  }
}
