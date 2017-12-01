package coop.rchain.caspersimulation.reporting

import coop.rchain.caspersimulation.TimeDependent
import coop.rchain.caspersimulation.identity.IdFactory

import scala.collection.mutable

/**
  * Records and outputs some property of a time dependent object as the state changes.
  * @tparam R return type of the dependent's state update
  * @tparam I type of the time dependent object
  * @tparam O type of the property the reporter is recording
  */
trait Reportable[R, I <: TimeDependent[R], O] {
  private[this] var roundCounter: Iterator[Int] = Iterator.from(1)
  val observations: mutable.HashMap[Int, O] = mutable.HashMap.empty[Int, O]

  /**
    * Record the observation for the current state of input.
    * @param input time dependent object to observe
    */
  final def record(input: I): Unit = memoize(roundCounter.next(), observe(input))

  def update(input: I)(implicit idf: IdFactory): R = {
    record(input)
    input.timeStep()
  }

  protected def memoize(round: Int, value: O): Unit = {
    observations += (round -> value)
  }

  /**
    * Combine two reporters into a single reporter
    * @param other reporter watching the same type of time dependent object, but recording
    *              a (possibly) different property
    * @tparam O2 type of the observations the other reporter is making
    * @return reporter which handles observations from both this and other.
    */
  final def and[O2](other: Reportable[R, I, O2]): Reportable[R, I, (O, O2)] = Reportable.both(this, other)

  /**
    * Forget any data this reporter has taken
    */
  def reset(): Unit = {
    roundCounter = Iterator.from(1)
    observations.clear()
  }

  /**
    * Map from the time dependent object to the property to keep track of.
    * @param input time dependent object to observe
    * @return value of the observed property
    */
  def observe(input: I): O

  /**
    * Method for writing the reporter's observations to a file
    * @param outputPath path to the output
    * @param suffix suffix to add to the filename (e.g. an index counter)
    */
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
