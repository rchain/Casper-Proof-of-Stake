package coop.rchain.caspersimulation.block

import coop.rchain.caspersimulation.dag.Node
import coop.rchain.caspersimulation.{Actor, Initializer, Validator}
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}
import coop.rchain.caspersimulation.onchainstate.{Diff, RChainState, Resource, Transaction}

import scala.collection.immutable.{HashMap, HashSet}

case class Block(
                  id: String,
                  creator: Actor,
                  parents: IndexedSeq[Block],
                  postState: RChainState,
                  transactions: IndexedSeq[Transaction],
                  receipts: IndexedSeq[(Diff, Int)],
                  justification: IndexedSeq[Block]
                ) extends Identifiable with Node[Block] {

  def fullDesc: String = toString + s"--($creator)[${parents.mkString(";")}]"

  //blocks conflict if their transactions overlap (in terms of being the same)
  //or if they result in Diffs which are incompatible
  /*
    TODO:
    Should probably change this so that it also checks that the
    histories since the first fork between this block and other do not conflict
   */
  def conflictsWith(other: Block): Boolean =
    transactions.exists(other.transactions.contains) || receipts.exists{
      case (diff1, _) =>
        other.receipts.exists{ case (diff2, _) => !Diff.compatible(diff1, diff2) }
    }

  override def value: Block = this
  override def neighbours: Iterator[Node[Block]] = parents.iterator
}

object Block {
  def apply(
             creator: Actor,
             parents: IndexedSeq[Block],
             postState: RChainState,
             transactions: IndexedSeq[Transaction],
             receipts: IndexedSeq[(Diff, Int)],
             justification: IndexedSeq[Block]
           )(implicit idf: IdFactory): Block =
    new Block(idf.next("Block-"), creator, parents, postState, transactions, receipts, justification)

  def genesis(bonds: HashMap[Validator, Int]): Block = {
    val id = "Genesis"
    val creator = Initializer
    val parents = IndexedSeq.empty[Block]
    val postState = RChainState(HashSet.empty[Resource], bonds)
    val transactions = IndexedSeq.empty[Transaction]
    val receipts = IndexedSeq.empty[(Diff, Int)]
    val justification = IndexedSeq.empty[Block]

    Block(id, creator, parents, postState, transactions, receipts, justification)
  }
}