package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.protocol.Ghost

object OffDagBranchLength extends CsvReporter[Unit, Network, (Int, Int)] {

  override val filename: String = "OffDagBranchLength"

  override def observe(input: Network): (Int, Int) = {
    val mainHead = Ghost.forkChoice(input.globalDag)
    val nonMainHeads = input.globalDag.heads - mainHead

    val offDagLength = nonMainHeads
      .map(
        //point each non-main block at its closest main DAG block
        b => b -> input.globalDag.greatestCommonParent(b, mainHead)
      )
      .map{
        //count the number of blocks between the non-main head and main parent
        case (start, end) => input.globalDag.pathLength(start, end).get
      }.sum

    //(total length) , (num non-main branches)
    offDagLength -> (input.globalDag.heads.size - 1)
  }

  override def toCsv: IndexedSeq[String] = {
    val rounds = observations.keys.toIndexedSeq.sorted

    "round,offDagLength,totalNonMainHeads" +: rounds.map(i => {
      s"$i,${observations(i)._1},${observations(i)._2}"
    })
  }
}
