package coop.rchain.caspersimulation.block

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.network.Synchronous
import coop.rchain.caspersimulation.onchainstate.{Diff, RChainState, Resource, Transaction}
import coop.rchain.caspersimulation.protocol.{BlockCreation, Ghost}

import scala.collection.immutable.{HashMap, HashSet}

object BlockDagScoringTest {
  def main(args: Array[String]): Unit = {
    implicit val idf: IdFactory = new IdFactory
    val network = Synchronous
    val dag = network.globalDag
    for(_ <- Iterator.range(0, 3)) network.createValidator

    val validators = network.validators.toIndexedSeq
    val stakes = IndexedSeq(3, 5, 7)
    val labels = IndexedSeq("A", "B", "C")

    val valMap = labels.zip(validators).toMap
    val stakesMap = labels.zip(stakes).toMap

    val bonds = validators.zip(stakes).foldLeft(HashMap.empty[Validator, Int]) {
      case (map, tuple) => map + tuple
    }

    val genesis = Block.genesis(bonds)
    val genMsg = BlockCreation(genesis)
    network.send(genMsg)

    val postState = RChainState(HashSet.empty[Resource], bonds)
    val transactions = IndexedSeq.empty[Transaction]
    val receipts = IndexedSeq.empty[(Diff, Int)]

    def createBlock(validator: Validator, parents: IndexedSeq[Block]): Block = {
      val block = Block(
        validator,
        parents,
        postState,
        transactions,
        receipts,
        dag.latestBlocks.toIndexedSeq
      )

      val msg = BlockCreation(block)
      network.send(msg)

      block
    }

    def assertScores(blocks: IndexedSeq[Block], answers: IndexedSeq[Int]): Unit = {
      val (scores, _, _) = dag.scoringAndChildren(dag.latestBlocks)
      blocks.map(scores).zip(answers).foreach{ case (x, y) =>
        assert(x == y)
      }
    }

    def assertHeadOrder(dag: BlockDag, answer: IndexedSeq[Block]): Unit = {
      val ordering = Ghost.orderedHeads(dag, dag.latestBlocks)
      assert(ordering == answer)
    }

    val a1 = createBlock(valMap("A"), IndexedSeq(genesis))
    val b1 = createBlock(valMap("B"), IndexedSeq(genesis))
    val c1 = createBlock(valMap("C"), IndexedSeq(genesis))

    assertScores(IndexedSeq(a1, b1, c1), stakes)
    assertHeadOrder(network.globalDag, IndexedSeq(c1, b1, a1))

    val a2 = createBlock(valMap("A"), IndexedSeq(a1))
    val b2 = createBlock(valMap("B"), IndexedSeq(b1, c1))

    //check implicit support is given when step parent is included
    assertScores(IndexedSeq(a2, b2), IndexedSeq(stakesMap("A"), stakesMap("B") + stakesMap("C")))
    assertHeadOrder(network.globalDag, IndexedSeq(b2, a2))

    val c2 = createBlock(valMap("C"), IndexedSeq(a2))

    //check implicit support is lost when conflicting block created
    assertScores(IndexedSeq(a2, b2, c2), IndexedSeq(stakesMap("A") + stakesMap("C"), stakesMap("B"), stakesMap("C")))
    assertHeadOrder(network.globalDag, IndexedSeq(c2, b2))
  }
}
