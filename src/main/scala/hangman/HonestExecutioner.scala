package hangman

import scala.util.Random

case class HonestExecutioner(private val secretWord: String) extends Executioner {
  override def answerGuess(trial: Trial, guess: Char): Set[Int] =
    secretWord.zipWithIndex.collect { case (`guess`, i) => i }.toSet

  override def revealSolution(trial: Trial): String = secretWord
}

object HonestExecutioner {
  def random(wordList: Iterable[String], wf: WordFill): HonestExecutioner = {
    val secretWord = Random.shuffle(wordList.view.filter(wf.matches)).head
    HonestExecutioner(secretWord)
  }
}

object HonestGame extends App {

  import ConsoleGame._

  val wordLength = readInt(s"Enter word length ($minLength to $maxLength): ", (minLength to maxLength).contains)
  val maxMistakes = readInt(s"Enter max mistakes (1 to 25): ", (1 to 25).contains)
  val initialTrial = Trial(WordFill(Seq.fill(wordLength)('_').mkString("")), maxMistakes)
  val honestExecutioner = HonestExecutioner.random(wordList, initialTrial.wordFill)
  playGame(initialTrial, honestExecutioner)
}