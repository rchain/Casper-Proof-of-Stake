package coop.rchain.caspersimulation.strategy.evolution

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.reporting.Reportable
import coop.rchain.caspersimulation.strategy.Strategy

import scala.math.round

case class Generation(id: String,
                         populationSize: Int,
                         validators: Set[Validator]) extends Identifiable {

  def nextGeneration[T](rounds: Int,
                     network: Network,
                     reporter: Reportable[Unit, Network, T],
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

    val aveFitness = validators.map(v => fitness(v)).sum / populationSize
    val newValidators = validators.flatMap(v => {
      val numChildren = round(fitness(v) / aveFitness).toInt
      Iterator.range(0, numChildren).map(_ => Validator(mutator(v.strategy), network))
    })

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