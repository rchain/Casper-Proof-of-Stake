package coop.rchain.caspersimulation.strategy.evolution

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.reporting.Reportable
import coop.rchain.caspersimulation.strategy.Strategy

import scala.math.{max, round}

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