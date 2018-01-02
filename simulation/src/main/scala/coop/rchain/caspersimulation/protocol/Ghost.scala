package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, DagUtil}

import scala.collection.immutable.HashSet

object Ghost {

  def forkChoice(blocks: IndexedSeq[Block]): Block = {

    val latestBlocks = DagUtil.latestBlocks(blocks, includeGenesis = false)

    val heads = DagUtil.heads(blocks)

    forkChoice(heads, latestBlocks)
  }

  def forkChoice(heads: Set[Block], latestBlocks: HashSet[Block]): Block = {
    //use id (proxy for hash) to break ties
    heads.maxBy(b => (score(b, latestBlocks), b.id))
  }

  def score(b: Block, latestBlocks: HashSet[Block] ): Double = {
    DagUtil.foldDag(0d, b, (scr: Double, blk: Block) => {
      if (latestBlocks.contains(blk)) scr + blk.weight else scr
    })
  }
}
