package coop.rchain.caspersimulation

import coop.rchain.caspersimulation.block.{Block, Genesis}
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.network.{Synchronous, UniformRandomDelay}
import coop.rchain.caspersimulation.protocol.PoliticalCapital
import coop.rchain.caspersimulation.reporting.{PoliticalCapitalFlow, RevFlow}
import coop.rchain.caspersimulation.strategy.{Human, ThresholdSpender}

object Simulation {
  def main(args: Array[String]): Unit = {
    implicit val idf: IdFactory = new IdFactory
    val maxTimeSteps: Int = 100

    val network = UniformRandomDelay(5)
    //val network = Synchronous

    val reporter = new PoliticalCapitalFlow() and new RevFlow()

    network.createUser

    //network.createValidator(Human)
    //network.createValidator(Human)
    val pc1 = new PoliticalCapital(Block.f * Block.f * Genesis.pca.amount)
    val pc2 = new PoliticalCapital(Block.f * Genesis.pca.amount) //probs the best
    val pc3 = new PoliticalCapital(2d * Block.f * Genesis.pca.amount)
    network.createValidator(ThresholdSpender(pc1))
    network.createValidator(ThresholdSpender(pc2))
    network.createValidator(ThresholdSpender(pc3))

    Iterator.range(0, maxTimeSteps).foreach(i => {
      reporter.update(network)
      println(i)
    })
    reporter.write("./output/")

    println(s"The winner is: ${network.validators.maxBy(_.revEarned)}")
  }
}
