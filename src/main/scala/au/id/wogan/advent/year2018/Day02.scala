package au.id.wogan.advent.year2018

import cats.data.Ior
import cats.kernel.Monoid

object Day02 extends AdventApp(2) {

  def go(): Unit = {
    println(s"Part One: ${checksum(input)}")
    println(s"Part Two: ${findPair(input.toList) map (common _).tupled getOrElse ""}")
  }

  val sum: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 1

    override def combine(x: Long, y: Long): Long = x * y
  }
  val longIsLong: Long <:< Long = implicitly

  def checksum(seq: Vector[String]): Long =
    seq foldMap countFor map (_.merge(longIsLong, sum)) getOrElse 0L

  def countFor(string: String): Option[Ior[Long, Long]] = {
    val counts = string.toList.groupBy(identity).mapValues(_.size).values.toSet
    val two = Option(1L) filter (_ => counts contains 2)
    val three = Option(1L) filter (_ => counts contains 3)
    Ior.fromOptions(two, three)
  }

  def findPair(list: List[String]): Option[(String, String)] =
    list.tails.toStream collectFirstSome {
      case head :: tail =>
        tail.find(compare(head, _)) map (head -> _)
      case _ =>
        None
    }

  def compare(first: String, second: String): Boolean =
    first.zip(second).map {
      case (a, b) if a == b => 0
      case _ => 1
    }.sum == 1

  def common(first: String, second: String): String =
    first.zip(second).collect {
      case (a, b) if a == b => a
    }.mkString
}
