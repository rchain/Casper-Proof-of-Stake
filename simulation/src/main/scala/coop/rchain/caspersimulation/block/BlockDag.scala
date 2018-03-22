package coop.rchain.caspersimulation.block

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.dag.{Dag, Node}

import scala.collection.mutable

class BlockDag extends Dag[Block, Block]{
  private val _blocks = new mutable.HashSet[Block]()
  private val _childlessBlocks = new mutable.HashSet[Block]()

  override def heads: mutable.HashSet[Block] = _childlessBlocks

  def add(b: Block): Unit = if (_blocks.contains(b)) {
    Unit //do nothing, already included
  } else if (b.parents.exists(p => !_blocks.contains(p))) {
    throw new Exception("Error: All parents of a block must already exist in the DAG")
  } else if (b.justification.exists(p => !_blocks.contains(p))) {
    throw new Exception("Error: All justifications of a block must already exist in the DAG")
  } else {
    _blocks += b //add new block to total set

    //remove blocks that are now no longer childless
    b.parents.filter(p => _childlessBlocks.contains(p)).foreach(_childlessBlocks -= _)

    //b must be childless because all parents of a block must already be in the DAG when it is added
    _childlessBlocks += b
  }

  def clear(): Unit = {
    _blocks.clear()
    _childlessBlocks.clear()
  }

  //returns 1 block for each creator (equivocation otherwise), which is "latest"
  //in the sense of not being in the justification of any other block by that
  //creator
  def latestBlocks: mutable.HashSet[Block] = {
    val result = new mutable.HashSet[Block]()

    _blocks
      .groupBy(_.creator)
      .mapValues(blks => {
        val unjustified = blks.clone()
        blks.foreach(_.justification.foreach(unjustified -= _))
        assert(unjustified.size == 1) //equivocation otherwise!
        unjustified
      })
      .valuesIterator
      .foreach(_.foreach(result += _))

    result
  }

  //return maps of blocks to their children and score, along with the genesis block
  //(used a precursor to GHOST)
  def scoringAndChildren(lblks: mutable.HashSet[Block]): (
      mutable.HashMap[Block, Int], //map of scores
      mutable.HashMap[Block, mutable.HashSet[Block]], //map of children
      Block //genesis block
    ) = {

    val chldMap = new mutable.HashMap[Block, mutable.HashSet[Block]]()
    val scrMap = new mutable.HashMap[Block, Int]()
    var genesis: Block = null

    _childlessBlocks.foreach(b => {
      chldMap += (b -> new mutable.HashSet[Block]())
    })

    //propagate scores for each latest block
    //(note that there is one latest block for each validator so this is equivalent to propagating validator scores)
    lblks.foreach(lb => lb.creator match {
      case v: Validator =>
        this.bfIterator(Some(lb)).foreach{ case Node(b) =>
          //for each block in the DAG of this latest block, increment its score
          val currScore = scrMap.getOrElse(b, 0)
          val newScore = currScore + b.parents.headOption
            .map(_.postState.bonds.getOrElse(v, 0)) //use parent block's bonds
            .getOrElse(b.postState.bonds.getOrElse(v, 0)) //unless it's genesis -- then there are no parents, so use itself
          scrMap.update(b, newScore)

          //Also keep track of parent-child relationships to minimize the number
          //of passes over the DAG. Note that the entire DAG will be visited since
          //the heads must be a subset of the latest messages.
          b.parents.foreach(p => {
            val children = chldMap.getOrElseUpdate(p, new mutable.HashSet[Block]())
            children += b
          })

        }
      case _ =>
        genesis = lb //genesis was not created by a validator
    })

    //add scores to the blocks implicitly supported through including a latest block as a "step parent"
    lblks.foreach(lb => {
      val children = chldMap(lb)
      children.foreach(c => {
        if (c.parents.length > 1 && c.creator != lb.creator) {
          val currScore = scrMap(c)
          val valWeight = lb.postState.bonds(lb.creator.asInstanceOf[Validator])
          scrMap.update(c, currScore + valWeight)
        }
      })
    })

    (scrMap, chldMap, genesis)
  }

}
