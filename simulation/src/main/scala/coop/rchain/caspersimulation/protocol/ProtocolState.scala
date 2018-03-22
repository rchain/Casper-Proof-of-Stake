package coop.rchain.caspersimulation.protocol

import coop.rchain.caspersimulation.block.{Block, BlockDag}
import coop.rchain.caspersimulation.onchainstate.Deploy

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class ProtocolState(msgHist: mutable.ArrayBuffer[Message]) {

  val blockDag = new BlockDag()

  //need this to hold on to blocks which cannot be included in the
  //dag yet because we don't yet know about the parents of the block
  private[this] val blockBuffer = new mutable.HashSet[Block]()

  def receive(msg: Message): Unit = {
    msgHist += msg
    msg match {
      case BlockCreation(b) =>
        Try(blockDag.add(b)) match {
          case Success(_) => Unit
          case Failure(_) => blockBuffer += b
        }
      case _ => Unit
    }

    //retry all the blocks in the waiting buffer
    blockBuffer
      .flatMap(b => Try(blockDag.add(b)).toOption.map(_ => b))
      .foreach(blockBuffer -= _)
  }

  def blockHist: Iterator[Block] = blockDag.bfIterator().map(_.value)

  def deployHist: IndexedSeq[Deploy] = msgHist.flatMap{
    case d: DeployRequest => Some(d.transaction)
    case _ => None
  }
}

object ProtocolState {
  def empty: ProtocolState = ProtocolState(mutable.ArrayBuffer.empty[Message])
}