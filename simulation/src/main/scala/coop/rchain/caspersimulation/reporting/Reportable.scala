package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.TimeDependent
import coop.rchain.caspersimulation.identity.IdFactory

import scala.collection.mutable

trait Reportable[R, I <: TimeDependent[R], O] {
  private[this] var roundCounter: Iterator[Int] = Iterator.from(1)
  val observations: mutable.HashMap[Int, O] = mutable.HashMap.empty[Int, O]

  def update(input: I)(implicit idf: IdFactory): R = {
    val observation: O = observe(input)
    memoize(roundCounter.next(), observation)
    input.timeStep()
  }

  def memoize(round: Int, value: O): Unit = {
    observations += (round -> value)
  }

  final def and[O2](other: Reportable[R, I, O2]): Reportable[R, I, (O, O2)] = Reportable.both(this, other)

  def reset(): Unit = {
    roundCounter = Iterator.from(1)
    observations.clear()
  }

  def observe(input: I): O

  def write(outputPath: String, suffix: String): Unit
}

object Reportable {
  def both[R, I <: TimeDependent[R], O1, O2](r1: Reportable[R, I, O1],
                                             r2: Reportable[R, I, O2]): Reportable[R, I, (O1, O2)] =
    new Reportable[R, I, (O1, O2)] {
      override def observe(input: I): (O1, O2) = (r1.observe(input), r2.observe(input))

      override def memoize(round: Int, value: (O1, O2)): Unit = {
        r1.memoize(round, value._1)
        r2.memoize(round, value._2)
      }

      override def reset(): Unit = {
        super.reset()
        r1.reset()
        r2.reset()
      }

      override def write(outputPath: String, suffix: String): Unit = {
        r1.write(outputPath, suffix)
        r2.write(outputPath, suffix)
      }
    }

}
