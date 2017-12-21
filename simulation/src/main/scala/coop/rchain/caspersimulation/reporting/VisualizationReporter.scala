package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.visualization.Gephi

/**
  * Reporter which outputs a (png) visualization of the
  * blogDAG at each observation and pdf file in the final
  * write step.
  * @param numValidators number of validators in the network the visualization is for
  * @param imagePath path where images from each observation are stored
  * @param layoutTime time to spend laying out the graph during each observation
  */
case class VisualizationReporter(numValidators: Int, imagePath: String, layoutTime: Double = 0.5)
  extends Reporter[Unit, Network, Unit] {

  Gephi.initialize(numValidators)

  override def observe(network: Network): Unit = {
    Gephi.addBlocks(network.validators.flatMap(_.state.blockHist))
    Gephi.formatGraph()
    Gephi.layoutGraph(layoutTime)
    Gephi.export(imagePath, "png", indexed = true)
  }

  override def write(outputPath: String, suffix: String): Unit = {
    Gephi.formatGraph()
    Gephi.layoutGraph(10) //spend 10s laying out the graph, should be long enough
    val extension = if (suffix.isEmpty) "pdf" else s"$suffix.pdf"
    Gephi.export(outputPath, extension)
  }

  //memoization not needed since each observation is simply Unit
  override def memoize(round: Int, value: Unit): Unit = Unit

  override def reset(): Unit = {
    super.reset()
    Gephi.reset(numValidators)
  }
}
