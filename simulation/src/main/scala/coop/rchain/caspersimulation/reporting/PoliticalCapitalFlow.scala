package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.network.Network
import coop.rchain.caspersimulation.protocol.PoliticalCapital

object PoliticalCapitalFlow extends ValidatorScalarFlow[PoliticalCapital] {

  override def observe(network: Network): Map[Validator, PoliticalCapital] = {
    network.validators.map(v => v -> v.state.pc).toMap
  }

  override val filename: String = "PCFlow"

  override val header: String = "validator,round,pc"

}
