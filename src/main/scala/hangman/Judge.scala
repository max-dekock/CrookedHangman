package hangman

sealed trait GameState

case class Trial(wordFill: WordFill, maxMistakes: Int) extends GameState

sealed trait Verdict extends GameState

case class Death(solution: String) extends Verdict

case class Freedom(solution: String) extends Verdict

case class Mistrial(explanation: String) extends Verdict

trait Executioner {
  def answerGuess(trial: Trial, guess: Char): Set[Int]

  def revealSolution(trial: Trial): String
}

trait Guesser {
  def guess(trial: Trial): Char
}

class Judge(val wordList: Seq[String]) {
  def hangmanRound(trial: Trial, guesser: Guesser, executioner: Executioner): Either[String, GameState] = {
    val guess = guesser.guess(trial)
    if (!Hangman.alphabet.contains(guess))
      Left("invalid letter")
    else if (trial.wordFill.allLetters.contains(guess))
      Left(s"letter '$guess' already guessed")
    else Right {
      val answer = executioner.answerGuess(trial, guess)
      val newWordFill = if (answer.isEmpty) trial.wordFill.addMistake(guess) else trial.wordFill.fillBlanks(guess, answer)
      newWordFill match {
        case None => Mistrial(s"illegal answer $answer in response to guess '$guess'")
        case Some(wf) if wf.completelyDefinesWord => Freedom(wf.blanks)
        case Some(wf) if wf.numMistakes < trial.maxMistakes => Trial(wf, trial.maxMistakes)
        case Some(wf) =>
          val solution = executioner.revealSolution(Trial(wf, trial.maxMistakes))
          if (!wf.matches(solution) || !wordList.contains(solution))
            Mistrial(s"executioner gave illegal solution $solution")
          else Death(solution)
      }
    }
  }
}
