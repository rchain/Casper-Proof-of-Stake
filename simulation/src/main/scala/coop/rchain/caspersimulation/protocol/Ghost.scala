package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, BlockDag}

import scala.annotation.tailrec
import scala.collection.mutable

object Ghost {

  def orderedHeads(dag: BlockDag, latestBlocks: mutable.HashSet[Block]): IndexedSeq[Block] = {
    val (scores, children, genesis) = dag.scoringAndChildren(latestBlocks)
    val decreasingOrder = Ordering[Int].reverse

    @tailrec
    def sortChildren(blks: IndexedSeq[Block]): IndexedSeq[Block] = {
      val newBlks = blks.flatMap(b => {
        val c = children(b)
        if(c.nonEmpty){
          c.toIndexedSeq.sortBy(scores)(decreasingOrder)
        } else {
          Some(b)
        }
      }).distinct
      if (newBlks == blks) {
        blks
      } else {
        sortChildren(newBlks)
      }
    }

    sortChildren(IndexedSeq(genesis))
  }

  def forkChoice(dag: BlockDag): Block = {
    val latestBlocks = dag.latestBlocks
    orderedHeads(dag, latestBlocks).head
  }
}
