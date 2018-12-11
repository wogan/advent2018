package au.id.wogan.advent.year2018

import scala.annotation.tailrec

object Day05 extends AdventApp(5) {

  val testInput = "dabAcCaCBAcCcaDA"

  override def go(): Unit = {
    val string = input.head
    val start = Cursor(0, string)
    val output = react(start).string
    println(s"Part One: ${output.length}")
    val best = units(string).toList map { unit =>
      val s = destroy(unit, string)
      println(s"$unit - ${s.substring(0, 10)}")
      react(Cursor(0, s))
    } minBy(_.string.length)
    println(s"Part Two: ${best.string.length}")
  }

  def units(string: String): Set[Char] =
    string.toUpperCase.toSet

  def destroy(unit: Char, string: String): String =
    string.replaceAll(s"[${unit.unit}]", "")

  @tailrec
  def react(c: Cursor): Cursor = {
    if (c.item.shouldReact) {
      react(c.zap)
    }else if (c.hasNext) {
      react(c.next)
    } else c
  }


  case class Cursor(pos: Int, string: String) {
    require(pos < string.length - 1)

    def item: (Char, Char) =
      string.charAt(pos) -> string.charAt(pos + 1)

    def zap: Cursor = {
      val newString = string.substring(0, pos) + string.substring(pos + 2)
      val newPos = Math.max(0, Math.min(pos -1, newString.length - 2))
      Cursor(newPos, newString)
    }

    def hasNext: Boolean =
      pos < string.length - 2

    def next: Cursor =
        Cursor(pos + 1, string)
  }

  implicit class CharTupleHelper(private val c: (Char, Char)) extends AnyVal {
    def shouldReact: Boolean =
      c._1.toLower == c._2.toLower && c._1 != c._2
  }

  implicit class CharHelper(private val c: Char) extends AnyVal {
    def unit: String =
      String.valueOf(Array(c.toUpper, c.toLower))
  }

}
