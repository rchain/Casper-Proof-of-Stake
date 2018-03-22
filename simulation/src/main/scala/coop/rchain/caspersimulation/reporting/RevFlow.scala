package coop.rchain.caspersimulation.reporting
import coop.rchain.caspersimulation.Validator
import coop.rchain.caspersimulation.network.Network

object RevFlow extends ValidatorScalarFlow[Int] {

  override def observe(input: Network): Map[Validator, Int] = {
    input.validators.map(v => v -> v.revEarned).toMap
  }

  override val filename: String = "RevFlow"

  override val observationName: String = "rev"

}
