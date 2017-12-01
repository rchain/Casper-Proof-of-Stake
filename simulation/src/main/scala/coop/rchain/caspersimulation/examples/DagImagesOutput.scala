package coop.rchain.caspersimulation.examples

import coop.rchain.caspersimulation.block.{Block, Genesis}
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.network.UniformRandomDelay
import coop.rchain.caspersimulation.protocol.PoliticalCapital
import coop.rchain.caspersimulation.reporting.{PoliticalCapitalFlow, RevFlow}
import coop.rchain.caspersimulation.strategy.ThresholdSpender
import coop.rchain.caspersimulation.visualization.Gephi

object DagImagesOutput {
  def main(args: Array[String]): Unit = {
    implicit val idf: IdFactory = new IdFactory
    val maxTimeSteps: Int = 100

    val network = UniformRandomDelay(5)

    val reporter = PoliticalCapitalFlow and RevFlow

    network.createUser

    val pc1 = new PoliticalCapital(Block.f * Block.f * Genesis.pca.amount)
    val pc2 = new PoliticalCapital(Block.f * Genesis.pca.amount)
    val pc3 = new PoliticalCapital(2d * Block.f * Genesis.pca.amount)
    network.createValidator(ThresholdSpender(pc1))
    network.createValidator(ThresholdSpender(pc2))
    network.createValidator(ThresholdSpender(pc3))

    Gephi.initialize(network.validators.size)
    Iterator.range(0, maxTimeSteps).foreach(i => {
      reporter.update(network)

      Gephi.addBlocks(network.validators.flatMap(_.state.blockHist))
      Gephi.formatGraph()
      Gephi.layoutGraph(0.5)
      Gephi.export("./output/img", "png", indexed = true)

      println(i)
    })
    reporter.write("./output", "")

    Gephi.formatGraph()
    Gephi.layoutGraph(10)
    Gephi.export("./output", "pdf")


    println(s"The winner is: ${network.validators.maxBy(_.revEarned)}")
  }

}
