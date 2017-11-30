package coop.rchain.caspersimulation

import coop.rchain.caspersimulation.identity.IdFactory

/**
  * Trait for an object who's state can change with time. Time is
  * measured in discrete steps.
  * @tparam T return type after time step (could be Unit for mutable
  *           state updates or some other type if the update is captured
  *           in a new object).
  */
trait TimeDependent[T] {
  /**
    * Perform the update for one time step
    * @param idf generator for labelling new objects created in process
    * @return result from update (Unit for mutable object state changes or
    *         some other object if a new object reflects the update)
    */
  def timeStep()(implicit idf: IdFactory): T
}
