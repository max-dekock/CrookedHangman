package hangman

trait Hangman {
  def answer(wf: WordFill, guess: Char): Set[Int]

  def revealSolution(wf: WordFill): String
}
