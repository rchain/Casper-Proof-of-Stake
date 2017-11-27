package coop.rchain.caspersimulation.identity

trait Identifiable extends scala.Equals {
  val id: String

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Identifiable]

  override def equals(obj: scala.Any): Boolean = canEqual(obj) && (obj match {
    case x: Identifiable => id == x.id
    case _ => false
  })

  override def hashCode(): Int = id.hashCode

  override def toString: String = id
}
