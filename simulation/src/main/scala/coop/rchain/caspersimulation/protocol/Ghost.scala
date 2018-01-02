package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, DagUtil}

object Ghost {

  def forkChoice(blocks: IndexedSeq[Block]): Block = {

    val heads = DagUtil.heads(blocks)

    forkChoice(heads)
  }

  def forkChoice(heads: Set[Block]): Block = {
    //use id (proxy for hash) to break ties
    heads.maxBy(b => (b.weight, b.id))
  }
}
