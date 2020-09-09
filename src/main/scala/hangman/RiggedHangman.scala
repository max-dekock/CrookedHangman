package hangman

import java.nio.file.{FileSystems, Files}

import scala.jdk.StreamConverters._
import scala.util.Random

case class RiggedHangman(wordList: Iterable[String], safeList: Seq[String]) extends Executioner {

  override def answerGuess(trial: Trial, guess: Char): Set[Int] = {
    val (has, hasnt) = safeList.filter(safe => safeMatches(safe, trial.wordFill)).partition(_.contains(guess))
    val blanks = Random.shuffle(Option.when(has.nonEmpty)(has).getOrElse(hasnt)).head
    blanks
      .zipWithIndex
      .collect {
        case (`guess`, i) => i
      }.toSet
  }

  def safeMatches(safe: String, wf: WordFill): Boolean = {
    safe.corresponds(wf.blanks) {
      case ('_', '_') => true
      case ('_', _) => false
      case (l, '_') => !wf.allLetters.contains(l)
      case (l1, l2) => l1 == l2
    }
  }

  override def revealSolution(trial: Trial): String = {
    Random.shuffle(wordList.view.filter(trial.wordFill.matches)).head
  }
}

object RiggedGame extends App {

  import ConsoleGame._

  val safeList: Vector[String] =
    Files.newBufferedReader(FileSystems.getDefault.getPath("safePartialWords.txt"))
      .lines()
      .toScala(Vector)

  val minLength = safeList.minBy(_.length).length
  val maxLength = safeList.maxBy(_.length).length

  do {
    val wordLength = readInt(s"Enter word length ($minLength to $maxLength): ", (minLength to maxLength).contains)
    val wf = WordFill(Seq.fill(wordLength)('_').mkString(""))
    val initialTrial = Trial(wf, 6)
    val executioner = RiggedHangman(wordList, safeList)
    playGame(initialTrial, executioner)
  } while (readYN("Play again? (y/n) "))
}