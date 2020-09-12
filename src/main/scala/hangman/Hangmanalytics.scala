package hangman

import java.io._

import hangman.Utils._

import scala.collection.concurrent
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source


object Hangmanalytics {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(8))
  private val binomMemo: concurrent.Map[(Int, Int), BigInt] = concurrent.TrieMap.empty

  def timeFunc[T](f: () => T, n: Int = 1, expectedVal: Option[T] = None): Double = {
    require(n > 0)
    val times: Seq[Double] = for (_ <- 0 until n) yield {
      val startTime: Double = System.nanoTime()
      val v: T = f()
      val elapsed: Double = System.nanoTime() - startTime
      expectedVal.foreach { ev => assert(v == ev) }
      elapsed
    }
    times.sum / n
  }

  def median(seq: Seq[Double]): Double = {
    require(seq.nonEmpty)
    val sortedSeq = seq.sorted
    val midpoint = sortedSeq.size / 2

    if (seq.size % 2 == 1) sortedSeq(midpoint)
    else (sortedSeq(midpoint) + sortedSeq(midpoint + 1)) / 2
  }

  def parEmpties(wl: Iterable[String], baseFill: WordFill, maxSize: Int): Future[Seq[Set[Char]]] = {
    val filtered = wl.view.filter(baseFill.matches).toSeq
    val letters = alphabet -- baseFill.allLetters
    parFindCombos(
      letters,
      { ls: Set[Char] => filtered.forall(w => w.exists(ls)) },
      maxSize
    )
  }

  def parFindCombos[T](elems: Set[T], p: Set[T] => Boolean, maxSize: Int): Future[Seq[Set[T]]] =
    Future.traverse(elems.subsets().takeWhile(_.size <= maxSize)) {
      es => Future {
        Option.when(p(es)) {
          es
        }
      }
    }.map(_.collect { case Some(es) => es }.toSeq)

  def groupedParEmpties(groupSize: Int)(wl: Iterable[String], baseFill: WordFill, maxSize: Int): Future[Seq[Set[Char]]] = {
    val filtered = wl.view.filter(baseFill.matches).toSeq
    val letters = alphabet -- baseFill.allLetters
    groupedParFindCombos(groupSize)(
      letters,
      { ls: Set[Char] => filtered.forall(w => w.exists(ls)) },
      maxSize
    )
  }

  def groupedParFindCombos[T](groupSize: Int)(elems: Set[T], p: Set[T] => Boolean, maxSize: Int): Future[Seq[Set[T]]] = {
    val groups: Iterator[Seq[Set[T]]] = elems.subsets().takeWhile(_.size <= maxSize).grouped(groupSize)
    Future.traverse(groups) { group => Future(group.filter(p)) }
  }.map(_ reduce (_ ++ _))

  def binomial(n: Int, k: Int): BigInt = {
    require(n >= k)
    require(k >= 0)
    if (k == 0 || k == n) BigInt(1)
    else binomMemo.getOrElseUpdate((n, k), {
      binomial(n - 1, k) + binomial(n - 1, k - 1)
    })
  }

  def combinations[T](ls: Seq[Seq[T]]): Seq[Seq[T]] = ls match {
    case Seq() => Seq(Seq())
    case head +: tail => val rec = combinations[T](tail)
      rec.flatMap(r => head.map(t => t +: r))
  }

  def main(args: Array[String]): Unit = {
    val wordList = loadWordList(Source.fromFile("wordlist_en.txt"))

    val f = Future.sequence(for {
      l <- 3 to 7
      wl = wordList.filter(_.length == l)
      empties = seqEmpties(wl, WordFill(Seq.fill(l)('_').mkString("")), 6)
      i <- 1 to l
    } yield {
      getSafeBlanks(wl, empties, 6, i).andThen {
        _.foreach { bls =>
          val fn = s"safes/l${l}_g6_$i"
          writeLines(fn, bls)
          println(s"wrote $fn")
        }
      }
    })

    Await.ready(f, 300.minutes)

  }

  def seqEmpties(wl: Iterable[String], baseFill: WordFill, maxSize: Int): Seq[Set[Char]] = {
    val filtered = wl.view.filter(baseFill.matches).toSeq
    val letters = alphabet -- baseFill.allLetters
    seqFindCombos(
      letters,
      { ls: Set[Char] => filtered.forall(w => w.exists(ls)) },
      maxSize
    )
  }

  def seqFindCombos[T](elems: Set[T], p: Set[T] => Boolean, maxSize: Int): Seq[Set[T]] =
    elems
      .subsets()
      .takeWhile(_.size <= maxSize)
      .filter(p)
      .toSeq

  def getSafeBlanks(wordList: Iterable[String], empties: Seq[Set[Char]], numGuesses: Int, numLetters: Int): Future[Vector[String]] = {
    val validWfs = getWfs(wordList, numLetters)
      .filter(wf => empties.forall(e => e.exists(wf.correctLetters.contains)))
    Future.traverse(validWfs) { wf =>
      Future {
        Option.when(isSafe(wordList, wf, numGuesses)) {
          println(wf.blanks)
          wf.blanks
        }
      }
    }.map(_.collect { case Some(bl) => bl }.toVector)
  }

  def isSafe(wordList: Iterable[String], wf: WordFill, n: Int): Boolean = {
    val filteredWords = wordList.filter(wf.matches)
    if (wf.blanks.count(_ == '_') == 1) {
      filteredWords.size > n
    } else {
      !(alphabet -- wf.allLetters)
        .subsets()
        .takeWhile(_.size <= n)
        .exists(ls => filteredWords.forall(w => w.exists(ls)))
    }
  }

  def getWfs(wordList: Iterable[String], n: Int): Iterator[WordFill] =
    wordList
      .iterator
      .flatMap { w =>
        WordFill.decompose(w)
          .toSet
          .subsets(n)
          .flatMap(wfs => WordFill.combine(wfs))
          .filter(!_.completelyDefinesWord)
      }
      .distinct

  def writeLines(filename: String, lines: Iterable[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line)
      bw.newLine()
    }
    bw.close()
  }
}
