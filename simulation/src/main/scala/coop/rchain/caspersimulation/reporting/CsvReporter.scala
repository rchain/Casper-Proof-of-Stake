package coop.rchain.caspersimulation.reporting

import java.io.PrintWriter

import coop.rchain.caspersimulation.TimeDependent

/**
  * Reporter who can write their observations to a csv file.
  * @tparam R return type of the dependent's state update
  * @tparam I type of the time dependent object
  * @tparam O type of the property the reporter is recording
  */
trait CsvReporter[R, I <: TimeDependent[R], O] extends Reporter[R, I, O] {
  val filename: String

  def toCsv: IndexedSeq[String]

  override def write(outputPath: String, suffix: String): Unit = {
    val outputFile: String = if (suffix.length > 0) {
      s"$outputPath/${filename}_$suffix.csv"
    } else {
      s"$outputPath/$filename.csv"
    }
    val out: PrintWriter = new PrintWriter(outputFile)
    toCsv.foreach(row => out.println(row))
    out.close()
  }
}
