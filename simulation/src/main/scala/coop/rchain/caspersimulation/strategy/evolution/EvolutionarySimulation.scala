package coop.rchain.caspersimulation.strategy.evolution

import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.{TimeDependent, Validator}
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.reporting.Reportable
import coop.rchain.caspersimulation.strategy.Strategy

/**
  * Simulate the evolution of a population of validators based on some fitness function and
  * mutation function. This simulation uses discrete generations of validators.
  * @param network Network the validators are part of
  * @param rounds Number of validation opportunities each validator has before
  *               network is reset with a new validator population
  * @param reporter Makes observations of properties of the network during each generation
  * @param fitness fitness function
  * @param mutator mutation function, i.e. how future strategies change from their parents
  * @param outputPath path to write reporter's observations to
  */
case class EvolutionarySimulation(network: Network,
                                     rounds: Int,
                                     reporter: Reportable[Unit, Network, _],
                                     fitness: (Validator) => Double,
                                     mutator: (Strategy) => Strategy,
                                     outputPath: String) extends TimeDependent[Unit] {

  private var _generation: Generation = _

  def currentGeneration: Generation = _generation

  /**
    * Set up simulation with initial population of validators
    * @param validators initial population of validators
    * @param idf id generator for labelling new objects created in initialization process
    */
  def init(validators: Set[Validator])(implicit idf: IdFactory): Unit = {
    _generation = Generation(validators)
  }

  /**
    * Run network with current generation for the number of rounds, then produces the next generation.
    * Data taken during the network run is written to a file.
    * @param idf id generator for labelling new objects created in process
    */
  override def timeStep()(implicit idf: IdFactory): Unit = {
    val genId = _generation.id
    _generation = _generation.nextGeneration(rounds, network, reporter, fitness, mutator)
    reporter.write(outputPath, genId)
  }
}
