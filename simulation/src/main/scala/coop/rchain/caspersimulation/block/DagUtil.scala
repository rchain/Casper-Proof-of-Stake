package coop.rchain.caspersimulation.block

import coop.rchain.caspersimulation.SmartContract

import scala.collection.immutable.HashSet
import scala.collection.mutable

object DagUtil {

  private def filter(blocks: IndexedSeq[Block], f: (Block) => Iterable[Block]): Set[Block] = {
    val result = mutable.HashSet(blocks: _*)
    if (result.size > 1) result.remove(Genesis)

    blocks.foreach(b => {
      f(b).foreach(p => if(result.contains(p)) result.remove(p))
    })

    result.toSet

  }

  //find blocks which are not the parent of any other known block
  def heads(blocks: IndexedSeq[Block]): Set[Block] = filter(blocks, _.parents)

  //find blocks which are not justified by any other known block
  def unjustified(blocks: IndexedSeq[Block]): Set[Block] = filter(blocks, _.justification)

  def foldDag[T](init: T, start: Block, comb: (T, Block) => T, end: Option[Block] = None): T = {
    start.toIterator(end).foldLeft(init)(comb)
  }

  def latestBlocks(blocks: IndexedSeq[Block], includeGenesis: Boolean = true): HashSet[Block] = {
    val validatorBlocks = HashSet(
      blocks
        .groupBy(_.creator)
        .mapValues(blks => {
          val latestBlks = DagUtil.unjustified(blks)
          assert(latestBlks.size <= 1) //equivocation otherwise!
          latestBlks
        })
        .values
        .flatten
        .toSeq: _*
    )

    if (includeGenesis) {
      validatorBlocks + Genesis
    } else {
      validatorBlocks
    }
  }

  def greatestCommonParent(a: Block, b: Block): Block = {
    val aParents = HashSet(a.toIterator(Some(b)).toSeq: _*)

    b.toIterator(Some(a)).find(blk => aParents.contains(blk)).getOrElse(Genesis)
  }

  def transactionsInDag(head: Block, end: Block, excludeEnd: Boolean): Set[SmartContract] = {
    val txns = new mutable.HashSet[SmartContract]()

    head.toIterator(Some(end)).foreach{
      case b: Transactions => b.txns.foreach(t => txns.add(t))
      case _ => Unit
    }

    if (excludeEnd && end.isInstanceOf[Transactions]){
      end.asInstanceOf[Transactions].txns.foreach(t => txns.remove(t))
    }

    txns.toSet
  }
}
