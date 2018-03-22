package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.block.Block
import coop.rchain.caspersimulation.dag.Node
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.onchainstate.{Deploy, Diff, RChainState, Transaction}

import scala.annotation.tailrec
import scala.collection.mutable

object estimator {
  def apply(v: Validator, state: ProtocolState)(implicit idf: IdFactory): Option[Block] = {
    /*
     * Logic:
     *  -Split protocol state into block messages and request messages
     *  -Score each of the blockDAG heads extracted from the block messages via GHOST
     *  -Let P = subset of heads such that P contains no conflicts and the total score is maximized
     *  -Let R = subset of request messages which are not included in DAG obtained by following blocks in P
     *  -If R is non-empty then create a new block with parents equal to P and (non-conflicting) txns obtained from R
     *  -Else if R is empty and |P| > 1 then create a block with parents equal to P and no transactions
     *  -Else None
     */

    val requests = new mutable.HashSet[Deploy]()
    state.deployHist.foreach(requests += _)

    //justification consists of all the most recent blocks I know about
    val latestBlocks = state.blockDag.latestBlocks

    val orderedHeads = Ghost.orderedHeads(state.blockDag, latestBlocks)

    //take as many blocks which do not conflict as possible,
    //starting with the higher scoring ones
    val parentBlocks = nonConflictingBlocks(orderedHeads.tail, IndexedSeq(orderedHeads.head))

    //capture the changes made by including all the transactions in the step parents
    val parentPoststate = parentBlocks.tail.foldLeft(parentBlocks.head.postState){
      case (st, block) =>
        block.transactions.foldLeft(st){
          case (s, t) => t.transform(s)._1
        }
    }

    //throw out all requests which have been already used
    state.blockDag.bfIterator(parentBlocks).foreach{ case Node(b) =>
      b.transactions.foreach{
        case d: Deploy => requests -= d
        case _ => Unit
      }
    }

    if (requests.isEmpty) {
      //no transactions which have not already been included
      if (parentBlocks.length > 1) {
        //including multiple parents is still worth publishing a block for
        val newBlock = Block(
          v,
          parentBlocks,
          parentPoststate,
          IndexedSeq.empty[Transaction],
          IndexedSeq.empty[(Diff, Int)],
          latestBlocks.toIndexedSeq
        )
        Some(newBlock)
      } else {
        None //nothing new to publish
      }
    } else {
      //pick out some non-conflicting transactions to put in the block
      val (finalPostState, txnsAndRcpts) = nonConflictingTransactions(
        requests.toIndexedSeq,
        IndexedSeq.empty[(Transaction, (Diff, Int))],
        parentPoststate
      )

      val (txns, recpts) = txnsAndRcpts.unzip(identity)
      val newBlock = Block(
        v,
        parentBlocks,
        finalPostState,
        txns,
        recpts,
        latestBlocks.toIndexedSeq
      )
      Some(newBlock)
    }
  }

  @tailrec
  private def nonConflictingBlocks(rem: IndexedSeq[Block], acc: IndexedSeq[Block]): IndexedSeq[Block] = {
    val nxt = rem.find(b => acc.forall(!_.conflictsWith(b)))
    nxt match {
      case None => acc
      case Some(b) => nonConflictingBlocks(rem.filter(_ == b), acc :+ b)
    }
  }

  @tailrec
  private def nonConflictingTransactions(
                                  rem: IndexedSeq[Transaction],
                                  acc: IndexedSeq[(Transaction, (Diff, Int))],
                                  currState: RChainState
                                ): (RChainState, IndexedSeq[(Transaction, (Diff, Int))]) = {
    if (rem.nonEmpty) {
      val (postState, diff, cost) = rem.head.transform(currState)
      if (acc.forall{ case (_, (prevDiffs, _)) => Diff.compatible(diff, prevDiffs) }) {
        nonConflictingTransactions(
          rem.tail,
          acc :+ (rem.head, (diff, cost)),
          postState
        )
      } else {
        nonConflictingTransactions(rem.tail, acc, currState)
      }
    } else {
      (currState, acc)
    }
  }
}
