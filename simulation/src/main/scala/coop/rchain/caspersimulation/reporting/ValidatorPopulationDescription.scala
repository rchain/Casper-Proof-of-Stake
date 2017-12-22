package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.strategy.evolution.EvolutionarySimulation

case class ValidatorPopulationDescription(descriptor: (Validator) => String, filename: String, descriptorName: String)
  extends CsvReporter[Unit, EvolutionarySimulation, Map[String, String]] {


  override def observe(input: EvolutionarySimulation): Map[String, String] = {
    input.currentGeneration.validators.map(v => v.id -> descriptor(v)).toMap
  }

  final override def toCsv: IndexedSeq[String] = {
    val header = s"generation,validator,$descriptorName"
    val generations = observations.keys.toIndexedSeq.sorted

    header +: generations.flatMap(i => {
      val obs = observations(i)
      val validators = obs.keys.toIndexedSeq.sorted
      validators.map(v => s"$i,$v,${obs(v)}")
    })
  }
}
