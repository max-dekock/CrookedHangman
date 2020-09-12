package hangman

import scala.util.Random

case class HonestHangman(private val secretWord: String) extends Hangman {
  override def answer(trial: WordFill, guess: Char): Set[Int] =
    secretWord.zipWithIndex.collect { case (`guess`, i) => i }.toSet

  override def revealSolution(trial: WordFill): String = secretWord
}

object HonestHangman {
  def withRandomWord(wordList: Iterable[String], wf: WordFill): HonestHangman = {
    val secretWord = Random.shuffle(wordList.view.filter(wf.matches)).head
    HonestHangman(secretWord)
  }
}

object HonestGame extends App {

  import ConsoleGame._

  do {
    val wordLength = readInt(s"Enter word length ($minLength to $maxLength): ", (minLength to maxLength).contains)
    val initialTrial = WordFill(Seq.fill(wordLength)('_').mkString(""))
    val honestHangman = HonestHangman.withRandomWord(wordList, initialTrial)
    playGame(initialTrial, honestHangman)
  } while (readYN("Play again? (y/n) "))

}