package hangman

import hangman.Utils._

import scala.collection.immutable

case class WordFill(blanks: String, mistakes: Set[Char] = Set.empty) {
  val correctLetters: Set[Char] = blanks.toSet - '_'
  val allLetters: Set[Char] = correctLetters ++ mistakes
  require(allLetters.forall(alphabet.contains))
  require((correctLetters & mistakes).isEmpty)

  def matches(word: String): Boolean = blanks.corresponds(word) {
    case ('_', l) => !allLetters.contains(l)
    case (bl, l) => l == bl
  }

  def length: Int = blanks.length

  def numUnfilled: Int = blanks.count(_ == '_')

  def completelyDefinesWord: Boolean = blanks.forall(_ != '_')

  def numMistakes: Int = mistakes.size

  def fillBlanks(letter: Char, indices: Iterable[Int]): Option[WordFill] =
    Option.when(!allLetters.contains(letter) && alphabet.contains(letter)) {
      val newBlanks = indices.foldLeft(Option(blanks)) {
        case (None, _) => None
        case (Some(str), i) if str(i) == '_' => Some(str.updated(i, letter))
        case _ => None
      }
      newBlanks.map(bl => WordFill(bl, mistakes))
    }.flatten

  def addMistake(letter: Char): Option[WordFill] =
    Option.when(alphabet.contains(letter) && !correctLetters.contains(letter)) {
      WordFill(blanks, mistakes + letter)
    }

  def addMistakes(letters: Iterable[Char]): Option[WordFill] =
    letters.foldLeft(Option(this)) {
      case (None, _) => None
      case (Some(wf), l) => wf.addMistake(l)
    }

  def compatible(other: WordFill): Boolean = this.blanks.corresponds(other.blanks) {
    case ('_', '_') => true
    case ('_', bl) => !this.allLetters.contains(bl)
    case (bl, '_') => !other.allLetters(bl)
    case (bl1, bl2) => bl1 == bl2
  }

  def combine(other: WordFill): Option[WordFill] = {
    val newBlanks: Option[String] = blanks.view.zip(other.blanks).foldLeft(Option("")) {
      case (None, _) => None
      case (Some(str), ('_', '_')) => Some(str appended '_')
      case (Some(str), (bl, bl2)) if bl == bl2 => Some(str appended bl)
      case (Some(str), ('_', bl)) if !this.allLetters.contains(bl) => Some(str appended bl)
      case (Some(str), (bl, '_')) if !other.allLetters.contains(bl) => Some(str appended bl)
      case _ => None
    }
    newBlanks.map(newBl => WordFill(newBl, this.mistakes ++ other.mistakes))
  }

  override def toString =
    f"WordFill(${'"'}$blanks${'"'}${if (mistakes.nonEmpty) ", {" ++ mistakes.mkString(",") ++ "}↦∅" else ""})"
}

object WordFill {
  def decompose(word: String): immutable.Iterable[WordFill] = {
    val allBlank = WordFill(blanks = Seq.fill(word.length)('_').mkString(""), mistakes = Set.empty)
    val indicesByLetter = word.indices.groupBy(word.charAt)
    indicesByLetter.flatMap {
      case (l, is) => allBlank.fillBlanks(l, is)
    }
  }

  def combine(wfs: Iterable[WordFill]): Option[WordFill] =
    wfs
      .view
      .map(Option(_))
      .reduceOption { (x: Option[WordFill], y: Option[WordFill]) =>
        (x, y) match {
          case (Some(wf1), Some(wf2)) => wf1.combine(wf2)
          case _ => None
        }
      }.flatten
}