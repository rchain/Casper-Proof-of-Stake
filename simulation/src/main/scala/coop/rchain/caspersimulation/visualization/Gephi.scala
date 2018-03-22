package coop.rchain.caspersimulation.visualization

import java.awt.Color
import java.io.File
import java.lang.Boolean.FALSE
import java.util.concurrent.TimeUnit

import org.gephi.appearance.plugin.palette.PaletteGenerator
import org.gephi.graph.api._
import org.gephi.io.exporter.api.ExportController
import org.gephi.layout.plugin.AutoLayout
import org.gephi.layout.plugin.forceAtlas.{ForceAtlas, ForceAtlasLayout}
import org.gephi.preview.api.{PreviewController, PreviewProperties, PreviewProperty}
import org.gephi.preview.types.EdgeColor
import org.gephi.project.api.ProjectController
import org.openide.util.Lookup

import scala.collection.mutable
import GephiUtil._
import coop.rchain.caspersimulation.block.{Block, BlockDag}
import coop.rchain.caspersimulation.protocol.Ghost

object Gephi {
  //controllers
  private val projectController = Lookup.getDefault.lookup(classOf[ProjectController])
  private var graphController: GraphController = _
  private var previewController: PreviewController = _
  private var exportController: ExportController = _

  private var index: Iterator[Int] = _ //output indexer

  private var model: GraphModel = _
  private var graph: DirectedGraph = _

  private var previewProperties: PreviewProperties = _ //properties for rendering
  private var palette: Iterator[Color] = _

  //determines size of node based on number of contracts in the block
  private val sizeFunc: (Int) => Float = (c: Int) => {
    if (c == 0) {
      attributeNodeSize
    } else {
      linearRange(1, 20, 5, 9)(c)
    }
  }

  //data in the graph
  private val dag: BlockDag = new BlockDag()
  private val nodes: mutable.HashMap[Block, Node] = mutable.HashMap.empty[Block, Node]
  private val edges: mutable.HashMap[(Block, Block), Edge] = mutable.HashMap.empty[(Block, Block), Edge]
  private val colors: mutable.HashMap[String, Color] = mutable.HashMap.empty[String, Color]

  def addBlocks(blocks: Iterable[Block]): Unit = {
    val existingBlocks = nodes.keySet
    val newBlocks = blocks.filter(b => !existingBlocks.contains(b))

    newBlocks.foreach(b => {
      dag.add(b)

      val n = model.factory().newNode(b.id)
      n.setAttribute("Creator", b.creator.id)
      if (!colors.contains(b.creator.id)) {
        colors += (b.creator.id -> palette.next())
      }
      val nContracts = b.transactions.length

      n.setAttribute("NumContracts", nContracts)
      nodes += (b -> n)
      graph.addNode(n)
    })

    newBlocks.foreach(b => b.parents.foreach(p => {
      val e = model.factory().newEdge(nodes(b), nodes(p), true)
      edges += ((b, p) -> e)
      graph.addEdge(e)
    }))
  }


  /**
    * Set up the workspace and required "controllers"
    * @param numValidators number of distinct validators creating blocks
    */
  def initialize(numValidators: Int): Unit = {
    projectController.newProject()
    graphController = Lookup.getDefault.lookup(classOf[GraphController])
    previewController = Lookup.getDefault.lookup(classOf[PreviewController])
    exportController = Lookup.getDefault.lookup(classOf[ExportController])

    index = Iterator.from(0)

    model = graphController.getGraphModel
    model.getNodeTable.addColumn("Creator", classOf[String])
    model.getNodeTable.addColumn("NumContracts", classOf[Int])
    model.getEdgeTable.addColumn("MainDAG", classOf[Boolean])
    graph = model.getDirectedGraph

    previewProperties = previewController.getModel.getProperties
    previewProperties.putValue(PreviewProperty.EDGE_COLOR, new EdgeColor(EdgeColor.Mode.ORIGINAL))
    previewProperties.putValue(PreviewProperty.EDGE_CURVED, FALSE)
    previewProperties.putValue(PreviewProperty.ARROW_SIZE, 5f)
    palette = PaletteGenerator
      .generatePalette(numValidators + 1, 10) //one extra colour for the genesis block
      .toIndexedSeq
      .sortBy(c => (c.getRed, c.getGreen, c.getBlue)) //sort to get consistent colour order
      .iterator

    dag.clear()
    nodes.clear()
    edges.clear()
    colors.clear()
  }

  /**
    * Reset to empty graph
    * @param numValidators number of distinct validators creating blocks
    */
  def reset(numValidators: Int): Unit = {
    projectController.closeCurrentWorkspace()
    projectController.closeCurrentProject()
    initialize(numValidators)
  }

  /**
    * Set sizes and colours of nodes and edges
    */
  def formatGraph(): Unit = {
    setElementColorByAttribute[String](nodes.valuesIterator, "Creator", colors)
    setNodeSizeByAttribute(nodes.valuesIterator, "NumContracts", sizeFunc)

    val dagHead = Ghost.forkChoice(dag) //mark the main DAG based on GHOST fork choice
    edges.valuesIterator.foreach(_.setAttribute("MainDAG", false))
    dag.bfIterator(Some(dagHead)).foreach(n => {
      val b = n.value
      b.parents.foreach(p => edges((b, p)).setAttribute("MainDAG", true))
    })
    setElementColorByAttribute[Boolean](edges.valuesIterator, "MainDAG", (b: Boolean) => {
      if (b) Color.RED else Color.BLACK
    })
  }

  /**
    * Sets node positions.
    * @param time amount of time (in seconds) to spend laying out the nodes.
    */
  def layoutGraph(time: Double): Unit = {
    val autoLayout = new AutoLayout((time * 1000).toLong, TimeUnit.MILLISECONDS)
    autoLayout.setGraphModel(model)

    val gather = new ForceAtlasLayout(new ForceAtlas)
    gather.setOutboundAttractionDistribution(true)
    gather.setSpeed(5d)

    autoLayout.addLayout(gather, 1f) //gather the nodes into something that resembles a chain
    autoLayout.execute()
  }

  /**
    * Print graph to a file
    * @param outputPath where to save the file
    * @param extension the type of file. One of pdf, png, gexf
    * @param indexed add an index after the filename? (Allows mulitple outputs to be created without overwriting)
    */
  def export(outputPath: String, extension: String, indexed: Boolean = false): Unit = {

    def filename: String = if (indexed) {
      s"$outputPath/blockDAG_${index.next().formatted("%04d")}.$extension"
    } else {
      s"$outputPath/blockDAG.$extension"
    }

    previewController.refreshPreview()
    exportController.exportFile(new File(filename))
  }
}
