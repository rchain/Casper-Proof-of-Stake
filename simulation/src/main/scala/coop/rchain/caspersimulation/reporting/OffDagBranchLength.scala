package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.block.DagUtil
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.protocol.Ghost

object OffDagBranchLength extends CsvReporter[Unit, Network, (Int, Int)] {

  override val filename: String = "OffDagBranchLength"

  override def observe(input: Network): (Int, Int) = {
    val blocks = input.validators.flatMap(_.state.blockHist)
    val dagHeads = DagUtil.heads(blocks)
    val mainHead = Ghost.forkChoice(dagHeads, DagUtil.latestBlocks(blocks.toIndexedSeq, includeGenesis = false))

    val offDagLength = dagHeads.iterator.filter(_ != mainHead)
      .map(
        //point each non-main block at its closest main DAG block
        b => b -> DagUtil.greatestCommonParent(b, mainHead)
      )
      .map{
        //count the number of blocks between the non-main head and main parent
        case (start, end) => start.toIterator(Some(end)).size - 1
      }.sum

    //(total length) , (num non-main branches)
    offDagLength -> (dagHeads.size - 1)
  }

  override def toCsv: IndexedSeq[String] = {
    val rounds = observations.keys.toIndexedSeq.sorted

    "round,offDagLength" +: rounds.map(i => {
      s"$i,${observations(i)._1},${observations(i)._2}"
    })
  }
}
