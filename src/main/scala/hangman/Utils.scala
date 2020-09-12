package hangman

import scala.io.Source
import scala.util.Random

object Utils {
  val alphabet: Set[Char] = ('a' to 'z').toSet

  val vowels: Set[Char] = "aeiouy".toSet

  val consonants: Set[Char] = alphabet -- vowels

  def loadWordList(source: Source): Vector[String] = {
    // Shuffling the list dramatically speeds up many search functions
    Random.shuffle(
      source
        .getLines
        .filter(w => w.length >= 2 && w.forall(alphabet) && w.exists(vowels))
        .toVector
    )
  }
}
