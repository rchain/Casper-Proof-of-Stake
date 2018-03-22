package coop.rchain.caspersimulation.dag

import scala.annotation.tailrec
import scala.collection.mutable

trait Dag[T, S <: Node[T]] {
  def heads: mutable.HashSet[S]

  private def iterator[C <: mutable.Seq[Node[T]]](
                                           toVisit: C,
                                           addToVisit: (Node[T]) => Unit,
                                           visit: () => Node[T],
                                           init: Iterator[Node[T]]
                                         ): Iterator[Node[T]] = new Iterator[Node[T]]{
    private[this] val visited = new mutable.HashSet[Node[T]]()
    init.foreach(n => addToVisit(n))

    @tailrec
    final override def hasNext: Boolean = toVisit.headOption match {
      case None => false
      case Some(nxt) => if (visited.contains(nxt)) {
        visit() //remove already visited block
        hasNext //try again to find existence of next block
      } else {
        true
      }
    }

    override def next(): Node[T] = if (hasNext) {
      val nxt = visit()
      visited.add(nxt)

      nxt.neighbours
        .filter(b => !visited.contains(b)) //only add parents that have not already been visited
        .foreach(b => addToVisit(b))

      nxt
    } else {
      Iterator.empty.next()
    }
  }


  def bfIterator(start: Iterable[Node[T]] = None): Iterator[Node[T]] = {
    val toVisit = new mutable.Queue[Node[T]]()
    val init = if(start.nonEmpty) start.iterator else heads.iterator
    iterator[mutable.Queue[Node[T]]](toVisit, toVisit.enqueue(_), () => toVisit.dequeue(), init)
  }

  def dfIterator(start: Iterable[Node[T]] = None): Iterator[Node[T]] = {
    val toVisit = new mutable.ArrayStack[Node[T]]()
    val init = if(start.nonEmpty) start.iterator else heads.iterator
    iterator[mutable.ArrayStack[Node[T]]](toVisit, toVisit.push, () => toVisit.pop(), init)
  }

  def greatestCommonParent(a: Node[T], b: Node[T]): Node[T] = {
    val aChain = new mutable.HashSet[Node[T]]()
    bfIterator(Some(a)).foreach(aChain += _)

    bfIterator(Some(b)).find(aChain.contains).get
  }

  def pathLength(a: Node[T], b: Node[T]): Option[Int] = {
    @tailrec
    def work(it: Iterator[(Node[T], Int)]): Option[Int] = if (it.hasNext){
      val (n, acc) = it.next()
      if (n == b) {
        Some(acc)
      } else {
        work(it ++ n.neighbours.map(_ -> (acc + 1)))
      }
    } else {
      None
    }

    work(Iterator.single(a -> 0))
  }
}
