package hangman

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random

object ConsoleGuesser extends Guesser {

  override def guess(trial: Trial): Char = {
    printTrial(trial)
    Iterator.continually {
      val input = readLine("Guess: ")
      if (input.length == 1) Some(input.head)
      else {
        println("Invalid input")
        None
      }
    }.collectFirst { case Some(guess) => guess }.get
  }

  def printTrial(trial: Trial): Unit = {
    val blanks = trial.wordFill.blanks
    val mistakes = trial.wordFill.mistakes
    val lost = trial.wordFill.numMistakes
    val remaining = trial.maxMistakes - lost

    val lifeBar = (Seq.fill(remaining)('♥') ++ Seq.fill(lost)('♡')).mkString("")

    println(lifeBar)
    println(blanks)
    println(mistakes.mkString(""))
  }
}

object ConsoleGame {
  val wordList: Seq[String] = Random.shuffle(Hangman.loadWordList(Source.fromFile("wordlist_en.txt")))
  val minLength: Int = wordList.minBy(_.length).length
  val maxLength: Int = wordList.maxBy(_.length).length

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

  def playGame(initialTrial: Trial, executioner: Executioner): Unit = {
    val guesser = ConsoleGuesser
    val judge = new Judge(wordList)

    val trialStates = scala.collection.mutable.Stack(initialTrial)

    val verdict = Iterator.continually {
      judge.hangmanRound(trialStates.head, guesser, executioner) match {
        case Left(msg) =>
          println(s"Invalid guess: $msg")
          None
        case Right(t: Trial) =>
          trialStates.push(t)
          None
        case Right(v: Verdict) => Some(v)
      }
    }.collectFirst { case Some(v) => v }.get

    verdict match {
      case Freedom(solution) =>
        println("You win!")
        println(s"The solution was $solution")
      case Death(solution) =>
        println("You died!")
        println(s"The solution was $solution")
      case Mistrial(explanation) =>
        println(s"Mistrial: $explanation")
    }
  }
}
