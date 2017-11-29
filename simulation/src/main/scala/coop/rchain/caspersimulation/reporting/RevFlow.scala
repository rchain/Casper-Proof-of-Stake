package coop.rchain.caspersimulation.reporting
import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.network.Network

object RevFlow extends ValidatorScalarFlow[Double] {

  override def observe(input: Network): Map[Validator, Double] = {
    input.validators.map(v => v -> v.revEarned).toMap
  }

  override def filename: String = "RevFlow"

  override def header: String = "validator,round,rev"

}
