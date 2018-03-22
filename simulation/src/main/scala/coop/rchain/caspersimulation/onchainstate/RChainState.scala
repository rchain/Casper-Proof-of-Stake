package coop.rchain.caspersimulation.onchainstate

import coop.rchain.caspersimulation.Validator

import scala.collection.immutable.{HashMap, HashSet}

case class RChainState(resources: HashSet[Resource], bonds: HashMap[Validator, Int]) {
  def without(removed: HashSet[Resource]): RChainState = RChainState(resources.diff(removed), bonds)
}
