package coop.rchain.caspersimulation.examples

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.block.Block
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.network.UniformRandomDelay
import coop.rchain.caspersimulation.protocol.BlockCreation
import coop.rchain.caspersimulation.reporting.{OffDagBranchLength, RevFlow, VisualizationReporter}

import scala.collection.immutable.HashMap

object DagImagesOutput {
  def main(args: Array[String]): Unit = {
    implicit val idf: IdFactory = new IdFactory
    val maxTimeSteps: Int = 10

    val network = UniformRandomDelay(5)

    network.createUser
    network.createUser
    network.createUser
    network.createUser
    network.createUser
    network.createUser
    network.createValidator
    network.createValidator
    network.createValidator

    val bonds = network.validators.map(_ -> 10).foldLeft(HashMap.empty[Validator, Int]) {
      case (map, tuple) => map + tuple
    }

    val genesis = Block.genesis(bonds)
    val genMsg = BlockCreation(genesis)
    network.globalDag.add(genesis)
    network.validators.foreach(_.newMessage(genMsg))

    val reporter = RevFlow
      .and(OffDagBranchLength)
      .and(VisualizationReporter(network.validators.size, "./output/img"))

    Iterator.range(0, maxTimeSteps).foreach(i => {
      reporter.update(network)

      println(i)
    })
    reporter.write("./output", "")


    println(s"The winner is: ${network.validators.maxBy(_.revEarned)}")
  }

}
