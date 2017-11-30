package coop.rchain.caspersimulation.strategy.evolution

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.reporting.Reportable
import coop.rchain.caspersimulation.strategy.Strategy

import scala.math.{max, round}

/**
  * A generation of validators for the evolutionary simulation.
  * @param id unique identifier
  * @param populationSize number of validators in the population
  * @param validators population of validators
  */
case class Generation(id: String,
                         populationSize: Int,
                         validators: Set[Validator]) extends Identifiable {

  /**
    * Run the population of validators on the network, then produce a new
    * generation of validators based on their fitness at the end of the run.
    * @param rounds Number of validation opportunities each validator has before
    *               network is reset with a new validator population
    * @param network Network the validators will work on
    * @param reporter Makes observations of properties of the network during the run
    * @param fitness fitness function; the number of children a validator gets in the
    *                next generation is proportional to their fitness.
    * @param mutator mutation function, i.e. how strategies in the next generation differ from this one
    * @param idf id generator for labelling new objects created in process
    * @return the next generation of validators
    */
  def nextGeneration(rounds: Int,
                     network: Network,
                     reporter: Reportable[Unit, Network, _],
                     fitness: (Validator) => Double,
                     mutator: (Strategy) => Strategy)(implicit idf: IdFactory): Generation = {
    reporter.reset()
    network.reset()
    network.createUser
    validators.foreach(v => network.addValidator(v))

    Iterator.range(0, rounds).foreach(i => {
      reporter.update(network)
      println(s"$id -- $i")
    })
    reporter.record(network) //record final state

    val fit = validators.map(v => v -> fitness(v)).toMap
    val mostFitValidator = fit.maxBy(_._2)._1
    val aveFitness = fit.valuesIterator.sum / populationSize
    val numChildren = fit.mapValues(f => round(f / aveFitness).toInt)
    val newPopSize = numChildren.valuesIterator.sum
    val newValidators = numChildren.flatMap{
      case (v, nc) =>
        val childGenerator = if (v == mostFitValidator) { //give most fit validator extra kids to prevent population decrease
          Iterator.range(0, nc + max(0, populationSize - newPopSize))
        } else {
          Iterator.range(0, nc)
        }
        childGenerator.map(_ => Validator(mutator(v.strategy), network))
    }.toSet

    Generation(newValidators)
  }

  //do group-by only once for performance reasons
  lazy val validatorsByStrategy: Map[Strategy, Set[Validator]] = validators.groupBy(_.strategy)

  def strategyProportions: Map[Strategy, Double] = {
    validatorsByStrategy.mapValues(_.size / populationSize)
  }

  def strategyAverageFitness(fitness: (Validator) => Double): Map[Strategy, Double] = {
    validatorsByStrategy.mapValues(vals => {
      vals.map(v => fitness(v)).sum / vals.size
    })
  }
}

object Generation {
  def apply(validators: Set[Validator])(implicit idf: IdFactory): Generation = {
    Generation(
      idf.next("Generation_"), validators.size, validators
    )
  }
}