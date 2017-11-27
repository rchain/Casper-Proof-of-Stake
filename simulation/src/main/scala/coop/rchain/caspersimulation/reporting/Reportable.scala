package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.network.TimeDependent

import scala.collection.mutable

trait Reportable[R, I <: TimeDependent[R], O] { self =>
  private[this] val roundCounter: Iterator[Int] = Iterator.from(1)
  val observations: mutable.HashMap[Int, O] = mutable.HashMap.empty[Int, O]

  def update(input: I)(implicit idf: IdFactory): R = {
    val observation: O = observe(input)
    memoize(roundCounter.next(), observation)
    input.timeStep()
  }

  def memoize(round: Int, value: O): Unit = {
    observations += (round -> value)
  }

  final def and[O2](other: Reportable[R, I, O2]): Reportable[R, I, (O, O2)] = new Reportable[R, I, (O, O2)] {
    override def observe(input: I): (O, O2) = (self.observe(input), other.observe(input))

    override def memoize(round: Int, value: (O, O2)): Unit = {
      self.memoize(round, value._1)
      other.memoize(round, value._2)
    }

    override def write(outputPath: String): Unit = {
      self.write(outputPath)
      other.write(outputPath)
    }
  }

  def observe(input: I): O

  def write(outputPath: String): Unit
}
