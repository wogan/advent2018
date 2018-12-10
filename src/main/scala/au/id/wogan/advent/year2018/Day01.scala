package au.id.wogan.advent.year2018

object Day01 extends AdventApp(1) {

  def go(): Unit = {
    val list = input.map(_.toLong)
    println(s"Part One: ${list.sum}")
    val firstRepeated = firstRepeatedFrequency(list)
    println(s"Part Two: $firstRepeated")
  }

  type E[A] = Either[Long, A]

  def firstRepeatedFrequency(input: Seq[Long]): Long =
    Stream.from(0).productR(input.toStream).scanLeft(0L)(_ + _).foldM[E, Set[Long]](Set()) {
      case (seen, next) =>
        if (seen contains next) {
          Left(next)
        } else {
          Right(seen + next)
        }
    }.left.get
}
