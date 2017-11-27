package coop.rchain.caspersimulation.strategy

import coop.rchain.caspersimulation.{SmartContract, Validator}
import coop.rchain.caspersimulation.block.{Block, DagUtil}
import coop.rchain.caspersimulation.identity.IdFactory
import coop.rchain.caspersimulation.protocol._

import scala.annotation.tailrec
import scala.io.Source.stdin
import scala.util.{Failure, Success, Try}

case object Human extends Strategy {
  def availableContracts(state: State): IndexedSeq[SmartContract] =
    Strategy.availableContracts(state).toIndexedSeq.sortBy(_.id)

  def viewAvailableContracts(state: State): Unit = println(
    optionsText(describe(availableContracts(state)))
  )

  def viewLatestBlocks(blockHist: IndexedSeq[Block]): Unit = println(
    optionsText(describe[Block](DagUtil.latestBlocks(blockHist).toIndexedSeq.sortBy(_.id), _.fullDesc))
  )

  def viewBlock(blockHist: IndexedSeq[Block]): Unit = {
    val question = "Enter the id of the block you would like to view:"
    val parseResponse = (s: String) => {
      val result = s.trim
      assert(result.length > 0)
      result
    }
    val id = freeInput(question, parseResponse)
    println(
      blockHist.find(_.id == id).map(_.fullDesc).getOrElse(s"You have not yet observed $id.")
    )
  }

  def viewRevBalances(blockHist: IndexedSeq[Block]): Unit =
    Strategy.revBalances(blockHist).toIndexedSeq.sortBy(_._2).foreach{
      case (v, rev) => println(s"$v: $rev")
    }

  override def pickContracts(validator: Validator): IndexedSeq[SmartContract] = {
    choose("Which contracts should go in this block?", describe(availableContracts(validator.state)))
  }

  override def pickAckBlocks(validator: Validator): IndexedSeq[Block] = {
    val blockHist = validator.state.blockHist
    val question = "Which blocks would you like to acknowledge?"
    val options = describe[Block](DagUtil.latestBlocks(blockHist).toIndexedSeq.sortBy(_.id), _.fullDesc)
    val constraint = (blks: IndexedSeq[Block]) => {
      if (blks.exists(blk => Strategy.previouslyAcknowledged(blk, validator))) {
        Right("Cannot acknowledge the same block twice! Try again.")
      } else if (blks.combinations(2).forall{ case Seq(a, b) => Strategy.isIndependent(a, b) }) {
        Left(blks)
      } else {
        Right("The chosen blocks must all be independent of one another! Try again.")
      }
    }

    constrainedChoose(question, options, constraint)
  }

  override def assignPC(pcBalance: PoliticalCapital,
                        blockData: Either[IndexedSeq[SmartContract], IndexedSeq[Block]]): PoliticalCapital = {
    val blockDataDesc = blockData match {
      case Left(contracts) => contracts.mkString("\n")
      case Right(blocks) => blocks.iterator.map(_.fullDesc).mkString("\n")
    }
    val question = s"Your PC balance is: $pcBalance.\n" +
      s"The block you are creating contains data:\n$blockDataDesc\n" +
      "How much political capital would you like to attach to the new block?"
    val parseResponse = (s: String) => {
      val pc = new PoliticalCapital(s.toDouble)
      assert(pc.amount <= pcBalance.amount)
      pc
    }

    freeInput(question, parseResponse)
  }

  private def act(action: Action, validator: Validator)(implicit idf: IdFactory): State = validator.state match {
    case State(`action`, _, _) => validator.state.act(validator)
    case _ => validator.state.changeIntent.act(validator)
  }


  private def printPCBalance(validator: Validator): Unit = println(validator.state.pc)

  override def stateUpdate(validator: Validator)(implicit idf: IdFactory): State = {
    val question = s"Ok, ${validator.id}, what would you like to do?"

    def menu(): State = {
      val options = IndexedSeq[(String, () => State)](
        "Nothing" -> (() => {validator.state}),
        "View my PC balance" -> (() => {printPCBalance(validator); menu()}),
        "View available contracts" -> (() => {viewAvailableContracts(validator.state); menu()}),
        "View latest blocks" -> (() => {viewLatestBlocks(validator.state.blockHist); menu()}),
        "View specific block" -> (() => {viewBlock(validator.state.blockHist); menu()}),
        "View REV balances" -> (() => {viewRevBalances(validator.state.blockHist); menu()}),
        "Propose" -> (() => act(Propose, validator)),
        "Acknowledge" -> (() => act(Acknowledge, validator))
      )
      chooseSingle(question, options)()
    }

    menu()
  }

  def describe[T](options: IndexedSeq[T], descriptor: (T) => String = (t: T) => {t.toString}): IndexedSeq[(String, T)] = {
    options.map(t => descriptor(t) -> t)
  }

  def optionsText[T](options: IndexedSeq[(String, T)]): String = if (options.isEmpty){
    "No options available!"
  } else {
    options.iterator
      .map{ case (desc, _) => desc }
      .zipWithIndex
      .map{ case (desc, i) => s"${i + 1}: $desc"}
      .mkString("\n")
  }

  def choose[T](question: String,
                options: IndexedSeq[(String, T)],
                prompt: String = "\nEnter space separated numbers for the options you would like:"): IndexedSeq[T] = if (options.isEmpty) {
    IndexedSeq.empty[T]
  } else {
    val optTxt = optionsText(options)

    @tailrec
    def getInput: IndexedSeq[T] = {
      println(question)
      println(optTxt)
      println(prompt)
      val response = stdin.getLines().next().split(" ").toIndexedSeq
      val result = Try(
        response.map(i => options(i.toInt - 1)._2)
      )
      result match {
        case Success(v) => v
        case Failure(_) =>
          println("Sorry, that input is invalid. Try again.")
          getInput
      }
    }

    getInput
  }

  def chooseSingle[T](question: String, options: IndexedSeq[(String, T)]): T = {
    @tailrec
    def getInput: T = {
      val result = choose(question, options, "\nEnter the number for the option you would like:")
      if (result.length > 1) {
        println("Choose only one option!")
        getInput
      } else {
        result.head
      }
    }

    getInput
  }

  def constrainedChoose[T](question: String,
                           options: IndexedSeq[(String, T)],
                           constraint: (IndexedSeq[T]) => Either[IndexedSeq[T], String]): IndexedSeq[T] = {
    @tailrec
    def getInput: IndexedSeq[T] = {
      val result = choose(question, options)
      constraint(result) match {
        case Left(t) => t
        case Right(errMessage) =>
          println(errMessage)
          getInput
      }
    }

    getInput
  }

  def freeInput[T](question: String, parse: (String) => T): T = {
    @tailrec
    def getInput: T = {
      println(question)
      val response = stdin.getLines().next()
      val result = Try(parse(response))
      result match {
        case Success(t) => t
        case Failure(_) =>
          println("Input is invalid! Try again.")
          getInput
      }
    }

    getInput
  }
}
