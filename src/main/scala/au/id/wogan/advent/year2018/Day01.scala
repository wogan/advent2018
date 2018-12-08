package au.id.wogan.advent.year2018

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

import scala.io.Source

object Day01 extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = Task {
    val lines = Source.fromResource("day01.txt").getLines()
    val list = lines.map(_.toLong).toVector
    println(s"Part One: ${list.sum}")
    val firstRepeated = firstRepeatedFrequency(list)
    println(s"Part Two: $firstRepeated")
    ExitCode.Success
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
