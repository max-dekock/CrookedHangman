package Hangman

import scala.io.Source
import scala.io.StdIn.readLine

trait Hangman {
  def guess(letter: Char): Either[String, Hangman]

  def alphabet: Set[Char] = ('a' to 'z').toSet

  def lettersGuessed: Set[Char]

  def numGuessesLeft: Int

  def blanks: Seq[Option[Char]]

  def solution: Option[String]

  def isFinished: Boolean = !isAlive || blanks.forall(o => o.isDefined)

  def isAlive: Boolean = numGuessesLeft > 0

  def wrongGuesses: Set[Char] = lettersGuessed -- blanks.flatten
}

object Hangman {
  def playOnConsole(hangman: Hangman): Unit = {
    var h = hangman
    while (!h.isFinished) {
      println(h.blanks.map(o => o.getOrElse('_')).mkString(" "))
      println(h.wrongGuesses.toSeq.sorted.mkString(""))
      println(s"${h.numGuessesLeft} guesses left")
      h.guess(readLine("Guess: ").charAt(0)) match {
        case Right(next) =>
          h = next
        case Left(error) =>
          println(s"Error: $error")
      }
    }
    if (h.isAlive)
      println("You win!")
    else
      println("You are dead!")
    println(s"The solution was ${h.solution.get}")
  }

  def loadWordList(source: Source): Vector[String] =
    source.getLines.filter(w => w.length > 2).toVector
}

object Main extends App {
  val vowels = Set('a', 'e', 'i', 'o', 'u', 'y')
  val wordList = Hangman.loadWordList(Source.fromFile("wordlist_en.txt")).filter(_.exists(vowels.contains))
  val h = new DishonestHangman(wordList, 8, 8)
  Hangman.playOnConsole(h)
}
