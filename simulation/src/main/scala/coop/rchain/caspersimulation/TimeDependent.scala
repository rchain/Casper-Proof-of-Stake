package coop.rchain.caspersimulation

import coop.rchain.caspersimulation.identity.IdFactory

trait TimeDependent[T] {
  def timeStep()(implicit idf: IdFactory): T
}
