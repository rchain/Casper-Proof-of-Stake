package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.strategy.Strategy
import coop.rchain.caspersimulation.strategy.evolution.EvolutionarySimulation

import scala.reflect.ClassTag

import scala.math.sqrt

case class StrategyDistributionFlow[S <: Strategy: ClassTag](parameter: (S) => Double, filename: String)
  extends CsvReportable[Unit, EvolutionarySimulation, (Double, Double)] {

  override def observe(input: EvolutionarySimulation): (Double, Double) = {
    val values = input.currentGeneration.validators.flatMap(v => v.strategy match {
      case s: S => Some(parameter(s))
      case _ => None
    })

    val n = values.size.toDouble
    val mean = values.sum / n
    val variance = values.map(x => x * x).sum / n - (mean * mean)

    (mean, sqrt(variance))
  }

  final def toCsv: IndexedSeq[String] = {
    "generation,mean,stddev" +:
      observations.toIndexedSeq.sortBy(_._1).map {
        case (round, (mean, sd)) => s"$round,$mean,$sd"
      }
  }

}
