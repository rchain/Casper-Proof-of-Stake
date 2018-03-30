package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, BlockDag}

import scala.annotation.tailrec
import scala.collection.mutable

object Ghost {

  def orderHeads(dag: BlockDag, latestBlocks: mutable.HashSet[Block]): IndexedSeq[Block] = {
    val (scores, children, genesis) = dag.scoringAndChildren(latestBlocks)
    val decreasingOrder = Ordering[Int].reverse

    @tailrec
    def sortChildren(blks: IndexedSeq[Block]): IndexedSeq[Block] = {
      val newBlks = blks.flatMap(b => {
        val empty = new mutable.HashSet[Block]()
        val c : mutable.HashSet[Block] = children.getOrElse(b, empty)
        if (c.nonEmpty) {
          c.toIndexedSeq.sortBy(scores)(decreasingOrder)
        } else {
          IndexedSeq(b)
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
    val latestBlocks: mutable.HashSet[Block] = dag.latestBlocks
    val orderedHeads : IndexedSeq[Block] = orderHeads(dag, latestBlocks)
    orderedHeads.head
  }
}
