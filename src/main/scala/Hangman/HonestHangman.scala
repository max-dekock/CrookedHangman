package Hangman

import scala.util.Random

class HonestHangman(protected val word: String,
                    val numGuessesLeft: Int,
                    val lettersGuessed: Set[Char] = Set()
                   ) extends Hangman {

  def this(wordList: Seq[String], numGuesses: Int) {
    this(wordList(Random.nextInt(wordList.size)), numGuesses)
  }

  override def guess(letter: Char): Either[String, Hangman] = {
    if (!alphabet.contains(letter))
      Left("invalid letter")
    else if (numGuessesLeft <= 0)
      Left("out of guesses")
    else if (lettersGuessed.contains(letter))
      Left("letter already guessed")
    else if (word contains letter)
      Right(new HonestHangman(word, numGuessesLeft, lettersGuessed + letter))
    else
      Right(new HonestHangman(word, numGuessesLeft - 1, lettersGuessed + letter))
  }

  override def blanks: Seq[Option[Char]] =
    word.map(c => if (lettersGuessed contains c) Some(c) else None)

  override def solution: Option[String] =
    if (isFinished) Some(word)
    else None
}