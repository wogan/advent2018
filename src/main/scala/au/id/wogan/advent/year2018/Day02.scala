package au.id.wogan.advent.year2018

import cats.data.Ior
import cats.effect.ExitCode
import cats.kernel.Monoid
import monix.eval.{Task, TaskApp}

import scala.io.Source

object Day02 extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = Task {
    val lines = Source.fromResource("day02.txt").getLines().toList
    println(s"Part One: ${checksum(lines)}")
    println(s"Part Two: ${findPair(lines) map (common _).tupled getOrElse ""}")
    ExitCode.Success
  }

  val sum: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 1

    override def combine(x: Long, y: Long): Long = x * y
  }
  val longIsLong: Long <:< Long = implicitly

  def checksum(seq: List[String]): Long =
    seq foldMap countFor map (_.merge(longIsLong, sum)) getOrElse 0L

  def countFor(string: String): Option[Ior[Long, Long]] = {
    val counts = string.toList.groupBy(identity).mapValues(_.size).values.toSet
    val two = Option(1L) filter (_ => counts contains 2)
    val three = Option(1L) filter (_ => counts contains 3)
    Ior.fromOptions(two, three)
  }

  type E[A] = Either[(String, String), A]

  // TODO: make tailrec
  def findPair(list: List[String]): Option[(String, String)] = {
    list match {
      case head :: tail =>
        val o = tail.find(compare(head, _))
        o map (head -> _) orElse findPair(tail)
      case _ =>
        None
    }
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
