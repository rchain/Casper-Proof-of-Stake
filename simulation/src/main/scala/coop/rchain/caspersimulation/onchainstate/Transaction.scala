package coop.rchain.caspersimulation.onchainstate

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

sealed abstract class Transaction extends Identifiable {
  type Cost = Int
  def transform(state: RChainState): (RChainState, Diff, Cost)
}

case class Deploy(id: String, resource: Resource) extends Transaction {
  override def transform(state: RChainState): (RChainState, Diff, Cost) = {
    val (postState, diff) = Deploy.transform(resource, state, ResourceDiff.empty)
    (postState, diff, 5 * diff.effected.size)
  }

}

object Deploy {
  def apply(resource: Resource)(implicit idf: IdFactory): Deploy = new Deploy(idf.next("Deploy-"), resource)

  @tailrec
  final def transform(resource: Resource, state: RChainState, runningDiff: ResourceDiff): (RChainState, ResourceDiff) =
    resource match {
      case c: Consume =>
        val produces = Resource.produces(state.resources)
        produces.find(p => Resource.matches(c, p)) match {
          case Some(p) =>
            val pair : HashSet[Resource] = HashSet(c, p)
            transform(c.continuation, state.without(pair), runningDiff + pair)
          case None =>
            val postState = RChainState(state.resources + c, state.bonds)
            val finalDiff = runningDiff + HashSet(c)
            (postState, finalDiff)
        }
      case p: Produce =>
        val consumes = Resource.consumes(state.resources)
        consumes.find(c => Resource.matches(c, p)) match {
          case Some(c) =>
            val pair: HashSet[Resource] = HashSet(c, p)
            transform(c.continuation, state.without(pair), runningDiff + pair)
          case None =>
            val postState = RChainState(state.resources + p, state.bonds)
            val finalDiff = runningDiff + HashSet(p)
            (postState, finalDiff)
        }
      case Stopped =>
        (state, runningDiff)
    }
}

case class Bond(id: String, validator: Validator, stake: Int) extends Transaction {
  override def transform(state: RChainState): (RChainState, Diff, Cost) = {
    val added = validator -> stake
    val postState = RChainState(state.resources, state.bonds + added)
    (postState, BondDiff(added), 1)
  }
}

case class UnBond(id: String, validator: Validator) extends Transaction {
  override def transform(state: RChainState): (RChainState, Diff, Cost) = {
    val postState = RChainState(state.resources, state.bonds - validator)
    (postState, UnBondDiff(validator), 1)
  }
}