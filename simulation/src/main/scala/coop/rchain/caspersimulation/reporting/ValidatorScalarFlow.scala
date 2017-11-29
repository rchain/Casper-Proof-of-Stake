package coop.rchain.caspersimulation.reporting

import java.io.PrintWriter

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.network.Network

abstract class ValidatorScalarFlow[S] extends Reportable[Unit, Network, Map[Validator, S]] {

  def filename: String
  def header: String

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
