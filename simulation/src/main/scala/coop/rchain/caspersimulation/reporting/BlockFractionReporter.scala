package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.block.{Acknowledgements, Transactions}
import coop.rchain.caspersimulation.network.Network

object BlockFractionReporter extends CsvReporter[Unit, Network, (Double, Double)] {

  override val filename: String = "blockFraction"

  override def toCsv: IndexedSeq[String] = {
    val header = "round,acknowledgments,proposals"
    val rounds: IndexedSeq[Int] = observations.keys.toIndexedSeq.sorted

    header +: rounds.map(i => {
      val (ack, prop) = observations(i)
      s"$i,$ack,$prop"
    })
  }

  override def observe(input: Network): (Double, Double) = {
    val blocks = input.validators.flatMap(_.state.blockHist)
    val (ackCount, propCount) = blocks.foldLeft((0L, 0L)){
      case ((ack, prop), b) => b match {
        case t: Transactions => (ack, prop + 1L)
        case a: Acknowledgements => (ack + 1L, prop)
        case _ => (ack, prop)
      }
    }
    val total  = (ackCount + propCount).toDouble

    if (total > 0) {
      (ackCount / total, propCount / total)
    } else {
      (0d, 0d)
    }
  }
}
