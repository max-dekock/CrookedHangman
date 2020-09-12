package hangman

import hangman.Judge._
import hangman.Utils._

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random

object ConsoleGame {
  val defaultMaxMistakes: Int = 6
  val wordList: Seq[String] = Random.shuffle(loadWordList(Source.fromFile("wordlist_en.txt")))
  val minLength: Int = wordList.minBy(_.length).length
  val maxLength: Int = wordList.maxBy(_.length).length

  def playGame(initialWf: WordFill, hangman: Hangman): Unit = {
    val judge = new Judge(wordList, defaultMaxMistakes)

    val wfs = scala.collection.mutable.Stack(initialWf)

    val outcome = Iterator.continually {
      val wf = wfs.head
      printTrialState(wf)
      val guess = readGuess(wf)
      judge.adjudicate(wf, guess, hangman) match {
        case Left(msg) =>
          println(s"Error: $msg")
          None
        case Right(v: Outcome) =>
          wfs.push(v.wf)
          Some(v)
        case Right(t: TrialState) =>
          wfs.push(t.wf)
          None
      }
    }.collectFirst { case Some(v) => v }.get

    printOutcome(outcome)
  }

  def printOutcome(outcome: Outcome, maxMistakes: Int = defaultMaxMistakes): Unit = {
    printTrialState(outcome.wf, maxMistakes)
    outcome match {
      case Death(_, solution) =>
        println(s"You died! The solution was '$solution'.")
      case Freedom(_, _) =>
        println(s"You win!")
      case Mistrial(_, explanation) =>
        println(s"Judge declares mistrial: $explanation.")
    }
  }

  def printTrialState(wf: WordFill, maxMistakes: Int = defaultMaxMistakes): Unit = {
    val blanks = wf.blanks
    val mistakes = wf.mistakes
    val lost = wf.numMistakes
    val remaining = maxMistakes - lost

    val lifeBar = (Seq.fill(remaining)('♥') ++ Seq.fill(lost)('♡')).mkString("")

    println(lifeBar)
    println(blanks)
    println(mistakes.mkString(""))
  }

  def readInt(prompt: String,
              validator: Int => Boolean = { _ => true }
             ): Int = Iterator.continually {
    readLine(prompt).toIntOption
      .filter(i => validator(i))
      .orElse {
        println("Invalid input")
        None
      }
  }.collectFirst { case Some(i) => i }.get

  def readYN(prompt: String): Boolean = Iterator.continually {
    readLine(prompt).toLowerCase match {
      case "y" | "yes" => Some(true)
      case "n" | "no" => Some(false)
      case _ => None
    }
  }.collectFirst { case Some(b) => b }.get

  def readGuess(wf: WordFill): Char = Iterator.continually {
    val input = readLine("Guess: ")
    if (input.length == 1) Some(input.head)
    else {
      println("Invalid input")
      None
    }
  }.collectFirst { case Some(guess) => guess }.get
}
