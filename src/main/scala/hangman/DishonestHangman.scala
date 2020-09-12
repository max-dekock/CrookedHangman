package hangman

import scala.util.Random

case class DishonestHangman(wordList: Seq[String], maxGuesses: Int) extends Hangman {

  override def answer(wf: WordFill, letter: Char): Set[Int] = {
    val (has, hasnt) = wordList.filter(wf.matches).partition(_.contains(letter))
    if (hasnt.nonEmpty)
      Set.empty
    else has.groupBy(w => w.indices.filter(i => w.charAt(i) == letter).toSet)
      .filter { case (k, _) => k.nonEmpty }
      .maxBy { case (_, v) => v.size }
      ._1
  }

  override def revealSolution(wf: WordFill): String = {
    val wl = wordList.filter(wf.matches)
    wl(Random.nextInt(wl.size))
  }
}

object DishonestGame extends App {

  import ConsoleGame._

  do {
    val wordLength = readInt(s"Enter word length ($minLength to $maxLength): ", (minLength to maxLength).contains)
    val initialTrial = WordFill(Seq.fill(wordLength)('_').mkString(""))
    val dishonestHangman = DishonestHangman(wordList, defaultMaxMistakes)
    playGame(initialTrial, dishonestHangman)
  } while (readYN("Play again? (y/n) "))
}