package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, DagUtil}

import scala.collection.immutable.HashSet

object Ghost {

  def forkChoice(blocks: IndexedSeq[Block]): Block = {

    val heads = DagUtil.heads(blocks)

    heads.maxBy(b => (b.weight, b.id)) //use id (proxy for hash) to break ties
  }
}
