package coop.rchain.caspersimulation.protocol

class PoliticalCapital(val amount: Double) extends AnyVal {
  def -(other: PoliticalCapital): PoliticalCapital = new PoliticalCapital(amount - other.amount)
  def +(other: PoliticalCapital): PoliticalCapital = new PoliticalCapital(amount + other.amount)

  override def toString: String = amount.toString
}
