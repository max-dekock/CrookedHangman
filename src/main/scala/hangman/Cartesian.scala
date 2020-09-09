package hangman

class Cartesian[T](private val ls: Seq[Seq[T]]) extends Iterator[Seq[T]] {

  private val last: Long = ls.map(_.size).product - 1
  private var iter: Long = 0

  override def hasNext(): Boolean = iter <= last

  override def next(): Seq[T] = {
    val res = combination(ls, iter)
    iter += 1
    res
  }

  def combination(xx: Seq[Seq[T]], i: Long): Seq[T] = xx match {
    case Seq() => Seq()
    case x +: xs => x((i % x.length).toInt) +: combination(xs, i / x.length)
  }
}

object Cartesian {
  def apply[T](ls: Seq[Seq[T]]): Iterator[Seq[T]] = new Cartesian(ls)
}
