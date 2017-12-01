package coop.rchain.caspersimulation.reporting

import java.io.PrintWriter

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.network.Network

/**
  * Reporter who creates a csv file with columns for the validator id, the round number and
  * a scalar value that is observed for each validator during each round. Note: a round
  * consists of each validator getting to perform a casper protocol action.
  * @tparam S type of the observed scalar
  */
abstract class ValidatorScalarFlow[S] extends CsvReporter[Unit, Network, Map[Validator, S]] {

  val header: String

  final def toCsv: IndexedSeq[String] = {
    val rounds: IndexedSeq[Int] = observations.keys.toIndexedSeq.sorted
    val validators: IndexedSeq[Validator] = observations
      .valuesIterator
      .map(_.keySet)
      .reduce(_ ++ _)
      .toIndexedSeq
      .sortBy(_.id)

    header +: validators.flatMap(validator => {
      val v = validator.toString
      rounds.iterator.map(r => s"$v,$r,${observations(r).getOrElse(validator, "")}")
    })
  }

  override def write(outputPath: String, suffix: String): Unit = {
    val outputFile: String = s"$outputPath/${filename}_$suffix.csv"
    val out: PrintWriter = new PrintWriter(outputFile)
    toCsv.foreach(row => out.println(row))
    out.close()
  }
}
