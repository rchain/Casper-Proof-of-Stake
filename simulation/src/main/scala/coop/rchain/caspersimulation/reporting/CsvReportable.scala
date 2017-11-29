package coop.rchain.caspersimulation.reporting

import java.io.PrintWriter

import coop.rchain.caspersimulation.TimeDependent

trait CsvReportable[R, I <: TimeDependent[R], O] extends Reportable[R, I, O] {
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
