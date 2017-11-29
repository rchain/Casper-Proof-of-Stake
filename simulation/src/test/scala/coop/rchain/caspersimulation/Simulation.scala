package coop.rchain.caspersimulation

import coop.rchain.caspersimulation.block.{Block, Genesis}
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.network.UniformRandomDelay
import coop.rchain.caspersimulation.protocol.PoliticalCapital
import coop.rchain.caspersimulation.reporting.{PoliticalCapitalFlow, Reportable, RevFlow}
import coop.rchain.caspersimulation.strategy.evolution.EvolutionarySimulation
import coop.rchain.caspersimulation.strategy.{Strategy, ThresholdSpender}

import scala.util.Random

object Simulation {
  def main(args: Array[String]): Unit = {
    implicit val idf: IdFactory = new IdFactory
    val rounds: Int = 100
    val network = UniformRandomDelay(5)
    val reporter = PoliticalCapitalFlow and RevFlow
    val numValidators = 10

    val fitness = (v: Validator) => {
      v.revEarned
    }

    val random = new Random()
    def gaussian(mean: Double, sd: Double): Double = random.nextGaussian() * sd + mean
    def delta: PoliticalCapital = new PoliticalCapital(
      gaussian(0d, 0.5d)
    )
    val mutator: (Strategy) => Strategy = {
      case t: ThresholdSpender => ThresholdSpender(t.threshold + delta)
      case s => s
    }

    val sim = EvolutionarySimulation(
      network, rounds, reporter,
      fitness, mutator, "./output/evolution"
    )

    val initValidators = Iterator.range(0, numValidators)
      .map(_ => gaussian(Block.f * Genesis.pca.amount, 2d * Block.f))
      .map(new PoliticalCapital(_))
      .toSet
      .map((t: PoliticalCapital) => Validator(ThresholdSpender(t), network))

    sim.init(initValidators)
    sim.timeStep()

  }
}
