package coop.rchain.caspersimulation.strategy

import coop.rchain.caspersimulation.{SmartContract, Validator}
import coop.rchain.caspersimulation.block.{Acknowledgements, Block, DagUtil, Transactions}
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol._

import scala.collection.mutable

abstract class Strategy {
  def pickContracts(validator: Validator): IndexedSeq[SmartContract]
  def assignPC(pcBalance: PoliticalCapital, blockData: Either[IndexedSeq[SmartContract], IndexedSeq[Block]]): PoliticalCapital
  def pickAckBlocks(validator: Validator): IndexedSeq[Block]
  def stateUpdate(validator: Validator)(implicit idf: IdFactory): State
}

object Strategy {
  def availableContracts(state: State): Set[SmartContract] = {
    val allContracts: mutable.HashSet[SmartContract] = mutable.HashSet.empty[SmartContract]

    state.contractHist.foreach{
      case c: SmartContract => allContracts.add(c)
      case _ => Unit //do nothing
    }

    val head = Ghost.forkChoice(state.blockHist)

    //walk through current main DAG to see if contract has been used
    head.toIterator().foreach{
      case b: Transactions =>
        b.txns.foreach(c => if(allContracts.contains(c)) allContracts.remove(c))

      case _ => Unit //do nothing
    }

    allContracts.toSet
  }

  //Two blocks are independent if neither is reachable from
  //the other and the transactions on their respective branches
  //do not conflict.
  def isIndependent(a: Block, b: Block): Boolean = {
    val gcp = DagUtil.greatestCommonParent(a, b)

    if (gcp == a || gcp == b) {
      false
    } else {
      val aTransactions = DagUtil.transactionsInDag(a, gcp, excludeEnd = true)
      val bTransactions = DagUtil.transactionsInDag(b, gcp, excludeEnd = true)

      aTransactions.intersect(bTransactions).isEmpty
    }
  }

  def maximalIndependentSubset(blocks: IndexedSeq[Block]): Set[Block] = {
    val sorted = blocks.sortBy(-_.pce.amount) //minus to sort in descending order
    val result = new mutable.HashSet[Block]()

    sorted.foreach(b => if (result.forall(isIndependent(_, b))) result += b)

    result.toSet
  }

  def previouslyAcknowledged(b: Block, v: Validator): Boolean = {
    v.state.msgHist.exists{
      case BlockCreation(block) if block.isInstanceOf[Acknowledgements] =>
        block.creator == v && block.asInstanceOf[Acknowledgements].blocks.contains(b)

      case _ => false
    }
  }

  def revBalances(blockHist: IndexedSeq[Block]): Map[Validator, Double] = {
    val result = new mutable.HashMap[Validator, Double]()

    val head = Ghost.forkChoice(blockHist)

    head.toIterator().foreach{
      case t: Transactions =>
        val rev = result.getOrElseUpdate(t.creator, 0d)
        result.update(t.creator, rev + t.txns.iterator.map(_.rev).sum)

      case _ => Unit
    }

    result.toMap
  }
}