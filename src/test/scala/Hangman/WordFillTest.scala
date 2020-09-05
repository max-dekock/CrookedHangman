package Hangman

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.io.Source

class WordFillTest extends AnyFlatSpec with should.Matchers {
  val WORDLIST: Seq[String] = Hangman.loadWordList(Source.fromFile("wordlist_en.txt"))

  "WordFill's constructor" should "throw IllegalArgumentException when blanks overlaps with mistakes" in {
    an[IllegalArgumentException] should be thrownBy {
      WordFill("__x", Set('x'))
    }
  }

  it should "produce IllegalArgumentException when given invalid letters" in {
    an[IllegalArgumentException] should be thrownBy {
      WordFill("$pring_ield", "ayu".toSet)
    }
    an[IllegalArgumentException] should be thrownBy {
      WordFill("tax", Set('_'))
    }
  }

  "An empty WordFill" should "match all words of the same length" in {
    val empty5 = WordFill("_____", Set.empty)
    val empty9 = WordFill("_________", Set.empty)
    val empty0 = WordFill("", Set.empty)
    assert(WORDLIST.filter(empty5.matchesWord) == WORDLIST.filter(_.length == 5))
    assert(WORDLIST.filter(empty9.matchesWord) == WORDLIST.filter(_.length == 9))
    assert(!WORDLIST.exists(empty0.matchesWord))
    assert(empty0.matchesWord(""))
  }

  "A completely filled WordFill" should "match exactly one word" in {
    val words = Seq("hairpin", "exes", "papaya", "ammonite", "zebras", "regolith")
    for (word <- words) {
      val wf = WordFill(word, Set.empty)
      assert(wf.completelyDefinesWord)
      val filtered = WORDLIST.filter(wf.matchesWord)
      assert(filtered.length == 1)
      assert(filtered.head == word)
    }
  }

  "A WordFill" should "not match any words containing mistakes" in {
    val wfs = Seq(
      WordFill("_____", Set('a')),
      WordFill("_______", "abc".toSet),
      WordFill("_a_a_a", "ntdfsq".toSet),
      WordFill("sh_t", Set('i'))
    )
    for (wf <- wfs) {
      val filtered = WORDLIST.filter(wf.matchesWord)
      val mistakes = wf.mistakes
      assert(!filtered.exists(_.forall(mistakes.contains)))
    }

    assert(!WORDLIST.exists(WordFill("____", Hangman.alphabet).matchesWord))
  }

  it should "only match words where all filled letters correspond exactly" in {
    assert(WordFill("__y__", Set.empty).matchesWord("chyme"))
    assert(!WordFill("__y__", Set.empty).matchesWord("phyly"))
    assert(WordFill("p_p___", Set.empty).matchesWord("papaya"))
    assert(!WordFill("p_p___", Set.empty).matchesWord("lapdog"))
    assert(WordFill("p__ph_l_", Set.empty).matchesWord("peephole"))
    assert(!WordFill("p__ph_l_", Set.empty).matchesWord("poophell"))
  }

  "Two WordFills" should "only combine if compatible" in {
    val wf1 = WordFill("_a_aya", Set('n', 't'))
    val wf2 = WordFill("p_p_y_", Set('t', 'r', 'q'))
    assert(wf1.compatible(wf2))
    wf1.combine(wf2) shouldBe Some(WordFill("papaya", Set('n', 't', 'r', 'q')))

    val wf3 = WordFill("_anana", Set('f', 'b'))
    assert(!wf1.compatible(wf3))
    wf1.combine(wf3) shouldBe None

    val wf4 = WordFill("____y_", Set('a'))
    assert(!wf1.compatible(wf4))
    wf1.combine(wf4) shouldBe None

    val wf5 = WordFill("____yr", Set('n'))
    assert(!wf2.compatible(wf5))
    wf2.combine(wf5) shouldBe None
  }


}
