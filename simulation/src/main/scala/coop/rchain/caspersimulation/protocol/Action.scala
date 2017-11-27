package coop.rchain.caspersimulation.protocol

sealed trait Action

case object Propose extends Action

case object Acknowledge extends Action