package Hangman

import scala.util.Random

class DishonestHangman(
                        protected val wordList: Seq[String],
                        val wordLength: Int,
                        val numGuessesLeft: Int,
                        val lettersGuessed: Set[Char],
                        val blanks: Seq[Option[Char]]
                      ) extends Hangman {

  def this(wordList: Seq[String], wordLength: Int, numGuesses: Int) =
    this(wordList.filter(_.length == wordLength), wordLength, numGuesses, Set(), Vector.fill(wordLength) {
      None
    })

  override def guess(letter: Char): Either[String, Hangman] =
    if (!alphabet.contains(letter))
      Left("invalid letter")
    else if (numGuessesLeft <= 0)
      Left("out of guesses")
    else if (lettersGuessed.contains(letter))
      Left("letter already guessed")
    else {
      val (has, hasnt) = wordList.partition(_.contains(letter))
      if (hasnt.nonEmpty)
        Right(new DishonestHangman(hasnt, wordLength, numGuessesLeft - 1, lettersGuessed + letter, blanks))
      else {
        val (is, newWordList) = has.groupBy(w => w.indices.filter(i => w.charAt(i) == letter).toSet)
          .filter(kv => kv._1.nonEmpty) // no empty indices
          .maxBy(kv => kv._2.size)
        val newBlanks = blanks.zipWithIndex.map(x => if (is.contains(x._2)) Some(letter) else x._1)
        Right(new DishonestHangman(newWordList, wordLength, numGuessesLeft, lettersGuessed + letter, newBlanks))
      }
    }

  override def solution: Option[String] = {
    if (isFinished) Some(wordList(Random.nextInt(wordList.size)))
    else None
  }
}