package hangman

import hangman.Utils._


object Judge {

  sealed trait TrialState {
    def wf: WordFill
  }

  sealed trait Outcome extends TrialState

  sealed abstract class Verdict(wf: WordFill, solution: String) extends Outcome

  case class InProgress(wf: WordFill) extends TrialState

  case class Mistrial(wf: WordFill, explanation: String) extends Outcome

  case class Death(wf: WordFill, solution: String) extends Verdict(wf, solution)

  case class Freedom(wf: WordFill, solution: String) extends Verdict(wf, solution)

}

class Judge(val wordList: Seq[String], val maxMistakes: Int = 6) {

  import Judge._

  def adjudicate(wf: WordFill, guess: Char, hangman: Hangman): Either[String, TrialState] =
    validateGuess(wf, guess).map { l =>
      val ans = hangman.answer(wf, l)
      fillInAnswer(wf, l, ans) match {
        case Some(newWf) if newWf.numMistakes >= maxMistakes =>
          validateSolution(newWf, hangman.revealSolution(newWf)) match {
            case Left(err) => Mistrial(newWf, err)
            case Right(solution) => Death(newWf, solution)
          }
        case Some(newWf) if newWf.completelyDefinesWord =>
          Judge.Freedom(newWf, newWf.blanks)
        case Some(newWf) => InProgress(newWf)
        case None => Mistrial(wf, "invalid answer from hangman")
      }
    }

  def validateGuess(wf: WordFill, guess: Char): Either[String, Char] = {
    if (!alphabet.contains(guess))
      Left("invalid letter")
    else if (wf.allLetters.contains(guess))
      Left(s"letter '$guess' already guessed'")
    else
      Right(guess)
  }

  def fillInAnswer(wf: WordFill, guess: Char, answer: Set[Int]): Option[WordFill] =
    if (answer.isEmpty)
      wf.addMistake(guess)
    else
      wf.fillBlanks(guess, answer)

  def validateSolution(wf: WordFill, solution: String): Either[String, String] =
    if (!wf.matches(solution))
      Left(s"solution '$solution' doesn't fit $wf")
    else if (!wordList.contains(solution))
      Left(s"solution '$solution' is not a word")
    else Right(solution)
}
