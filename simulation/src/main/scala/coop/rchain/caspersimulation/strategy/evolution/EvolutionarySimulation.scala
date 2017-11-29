package coop.rchain.caspersimulation.strategy.evolution

import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.{TimeDependent, Validator}
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.reporting.Reportable
import coop.rchain.caspersimulation.strategy.Strategy

case class EvolutionarySimulation(network: Network,
                                     rounds: Int,
                                     reporter: Reportable[Unit, Network, _],
                                     fitness: (Validator) => Double,
                                     mutator: (Strategy) => Strategy,
                                     outputPath: String) extends TimeDependent[Unit] {

  private var _generation: Generation = _

  def currentGeneration: Generation = _generation

  def init(validators: Set[Validator])(implicit idf: IdFactory): Unit = {
    _generation = Generation(validators)
  }

  override def timeStep()(implicit idf: IdFactory): Unit = {
    _generation = _generation.nextGeneration(rounds, network, reporter, fitness, mutator)
    reporter.write(outputPath, _generation.id)
  }
}
