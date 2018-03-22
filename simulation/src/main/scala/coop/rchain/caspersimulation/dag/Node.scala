package coop.rchain.caspersimulation.dag

trait Node[A] {
  def value: A
  def neighbours: Iterator[Node[A]]
}

object Node {
  def unapply[A](node: Node[A]): Option[A] = Some(node.value)
}
