package au.id.wogan.advent.year2018

import java.lang.Math.max

import cats.kernel.Monoid

import scala.util.matching.Regex

object Day03 extends AdventApp(3) {
  type Cell = (Int, Int)

  case class Square(left: Int, top: Int, width: Int, height: Int) {
    val right: Int = left + width
    val bottom: Int = top + height

    def cells: Vector[Cell] =
      Vector.range(left, right).flatMap(l => Vector.range(top,bottom) map (l -> _))
  }

  case class Claim(id: Long, square: Square)

  val regex: Regex = "#(?<id>\\d+) @ (?<left>\\d+),(?<top>\\d+): (?<width>\\d+)x(?<height>\\d+)".r

  def parse(line: String): Claim =
    regex.findFirstMatchIn(line).map { implicit m =>
      Claim('id, Square('left, 'top, 'width, 'height))
    }.get

  def go(): Unit = {
    val claims = input map parse
    val overlap = fill(claims map (_.square)).values.count(_ > 1)
    println(s"Part One: $overlap")
    val u = unique(owners(claims)) filter (_._2 == 1)
    assert(u.size == 1)
    println(s"Part Two: ${u.head._1}")
  }

  def fill(seq: Seq[Square]): Map[Cell, Int] =
    seq.toVector.flatMap(_.cells).groupBy(identity).mapValues(_.size)

  def owners(seq: Seq[Claim]): Map[Cell, Seq[Claim]] =
    seq.toVector.flatMap(c => c.square.cells map (c -> _)).groupBy(_._2).mapValues(_ map (_._1))

  val maxM: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = max(x, y)
  }

  def unique(owners: Map[Cell, Seq[Claim]]): Map[Claim, Int] =
    owners.toList.foldMap {
      case (_, claims) =>
        claims.toList.map(_ -> claims.size).toMap
    }(cats.instances.map.catsKernelStdMonoidForMap(maxM))
}
