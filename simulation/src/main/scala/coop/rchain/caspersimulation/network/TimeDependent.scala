package coop.rchain.caspersimulation.network

import coop.rchain.caspersimulation.identity.IdFactory

trait TimeDependent[T] {
  def timeStep()(implicit idf: IdFactory): T
}
