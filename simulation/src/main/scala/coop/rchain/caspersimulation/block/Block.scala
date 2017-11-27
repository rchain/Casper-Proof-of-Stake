package coop.rchain.caspersimulation.block

import coop.rchain.caspersimulation.{Actor, Initializer, SmartContract, Validator}
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}
import coop.rchain.caspersimulation.protocol.PoliticalCapital

import scala.collection.mutable

sealed abstract class Block extends Identifiable { self =>
  val creator: Actor
  val parents: IndexedSeq[Block]
  val pca: PoliticalCapital //Political Capital Attached
  val justification: IndexedSeq[Block]

  def weight: Double
  def pce: PoliticalCapital //Political Capital Earned

  def fullDesc: String

  def toIterator(end: Option[Block] = None): Iterator[Block] = new Iterator[Block]{
    private[this] val visitedBlocks = new mutable.HashSet[Block]()
    private[this] val queue = new mutable.Queue[Block]()
    queue.enqueue(self)

    override def hasNext: Boolean = queue.nonEmpty

    override def next(): Block = if (hasNext) {
      val nxt = queue.dequeue()
      visitedBlocks.add(nxt)
      if (!end.contains(nxt)) { //only add the parents if we have not yet reached the end
        nxt.parents.iterator.filter(b => !visitedBlocks.contains(b)).foreach(b => queue.enqueue(b))
      }
      nxt
    } else {
      Iterator.empty.next()
    }
  }
}

object Block {
  val f: Double = 0.4 //the fraction used in calculating weights and PC earnings
}

case object Genesis extends Block {
  override val id: String = "Genesis"
  override val creator: Actor = Initializer
  override val parents: IndexedSeq[Block] = IndexedSeq.empty[Block]
  override val pca: PoliticalCapital = new PoliticalCapital(10d) //initial amount of political capital
  override val justification: IndexedSeq[Block] = IndexedSeq(Genesis) //genesis justifies itself (...somehow)

  override def weight: Double = pca.amount

  override def pce: PoliticalCapital = new PoliticalCapital(Block.f * pca.amount)

  override def fullDesc: String = toString()
}

case class Transactions(id: String,
                        creator: Validator,
                        parents: IndexedSeq[Block],
                        pca: PoliticalCapital,
                        txns: IndexedSeq[SmartContract],
                        justification: IndexedSeq[Block]) extends Block {
  override def weight: Double = pca.amount

  override def pce: PoliticalCapital = new PoliticalCapital(Block.f * pca.amount)

  override def fullDesc: String = s"[${parents.mkString(", ")}]${super.toString}($creator -- $pca){${txns.mkString(", ")}}"
}

object Transactions {
  def apply(creator: Validator,
            parents: IndexedSeq[Block],
            pca: PoliticalCapital,
            txns: IndexedSeq[SmartContract],
            justification: IndexedSeq[Block])(implicit idf: IdFactory): Transactions =
    Transactions(idf.next("block_"), creator, parents, pca, txns, justification)
}

case class Acknowledgements(id: String,
                            creator: Validator,
                            blocks: IndexedSeq[Block],
                            override val pca: PoliticalCapital,
                            override val justification: IndexedSeq[Block]) extends Block {

  override val parents: IndexedSeq[Block] = blocks

  //memoize recursive function as an optimization
  private[this] lazy val _weight = pca.amount + (Block.f * blocks.iterator.map(_.weight).sum)
  override def weight: Double = _weight

  //memoize recursive function as an optimization
  private[this] lazy val _pce =  new PoliticalCapital(
    Block.f * (pca.amount + blocks.iterator.map(_.pce.amount).sum)
  )
  override def pce: PoliticalCapital = _pce

  override def fullDesc: String = s"[${blocks.mkString(", ")}]${super.toString}($creator -- $pca){${blocks.mkString(", ")}}"
}

object Acknowledgements {
  def apply(creator: Validator,
            blocks: IndexedSeq[Block],
            pca: PoliticalCapital,
            justification: IndexedSeq[Block])(implicit idf: IdFactory): Acknowledgements = {
    Acknowledgements(idf.next("block_"), creator, blocks, pca, justification)
  }
}

