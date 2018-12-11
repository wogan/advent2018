package au.id.wogan.advent.year2018

import Math.{max, abs}

object Day06 extends AdventApp(6) {

  val testInput: Vector[String] =
    """1, 1
      |1, 6
      |8, 3
      |3, 4
      |5, 5
      |8, 9""".stripMargin.lines.toVector

  type Point = (Int, Int)

  override def go(): Unit = {
    val points = input map parse
    val size = sizeOf(points)
    val (maxPoint, elements) = positions(size).toVector foldMap (closestTo(_, points)) filterNot {
      _._2 exists (_.isEdge(size)) // remove infinite groups
    } maxBy (_._2.size)
    println(s"Part One: Point $maxPoint has ${elements.size} area")

    val totals = positions(size) map (p => points.map(_ distanceTo p).sum) filter (_ < 10000)
    println(s"Part Two: Size of size region: ${totals.size}")

  }

  implicit class PointMethods(private val pos: Point) extends AnyVal {
    def x: Int = pos._1

    def y: Int = pos._2

    def distanceTo(location: (Int, Int)): Int =
      abs(x - location._1) + abs(y - location._2)

    def isEdge(max: (Int, Int)): Boolean =
      x == 0 || y == 0 || x == max.x || y == max.y
  }

  def parse(line: String): Point =
    line.split(", ").toList map (_.toInt) match {
      case x :: y :: Nil =>
        x -> y
      case _ => throw new IllegalArgumentException
    }

  def sizeOf(points: Seq[Point]): (Int, Int) =
    points.foldLeft(0 -> 0) {
      case ((mX, mY), (x, y)) =>
        max(x, mX) -> max(y, mY)
    }

  def closestTo(pos: (Int, Int), points: Seq[Point]): Map[Point, List[Point]] = {
    val distances = points.toList fproduct (_.distanceTo(pos))
    val min = distances.map(_._2).min
    distances filter (_._2 == min) match {
      case (point, _) :: Nil =>
        Map(point -> List(pos))
      case _ =>
        Map.empty
    }
  }

  def positions(size: (Int, Int)): Seq[(Int, Int)] =
    0 to size._1 flatMap (x => 0 to size._2 map (x -> _))

  // If you hit the "edge" (0, y), (x, 0), (maxX, y) or (x, maxY) then exclude
}
