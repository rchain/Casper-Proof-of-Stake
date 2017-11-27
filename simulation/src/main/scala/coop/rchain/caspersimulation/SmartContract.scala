package coop.rchain.caspersimulation

import coop.rchain.caspersimulation.identity.{IdFactory, Identifiable}

case class SmartContract(id: String, rev: Double) extends Identifiable {
  override def toString: String = s"${super.toString}($rev rev)"
}

object SmartContract {
  def apply(rev: Double)(implicit idf: IdFactory): SmartContract =
    SmartContract(idf.next("contract_"), rev)
}
