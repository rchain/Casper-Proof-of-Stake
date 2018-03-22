package coop.rchain.caspersimulation.onchainstate

import coop.rchain.caspersimulation.Validator

import scala.collection.immutable.HashSet

sealed abstract class Diff

object Diff {
  def compatible(a: Diff, b: Diff): Boolean = a match {
    case ResourceDiff(aEffected) => b match {
      case ResourceDiff(bEffected) => aEffected.intersect(bEffected).isEmpty
      case _ => true
    }
    case BondDiff(_) => b match {
      case BondDiff(_) => false //bond diffs are never compatible
      case _ => true
    }
    case UnBondDiff(_) => b match {
      case UnBondDiff(_) => false
      case _ => true
    }
  }
}

case class ResourceDiff(effected: HashSet[Resource]) extends Diff {
  def +(resources: HashSet[Resource]): ResourceDiff = ResourceDiff(effected.union(resources))
}
object ResourceDiff {
  def empty: ResourceDiff = ResourceDiff(HashSet.empty[Resource])
}

case class BondDiff(added: (Validator, Int)) extends Diff
case class UnBondDiff(removed: Validator) extends Diff
