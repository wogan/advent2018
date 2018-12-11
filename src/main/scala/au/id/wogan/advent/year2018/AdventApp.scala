package au.id.wogan.advent.year2018

import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.temporal.ChronoField.{HOUR_OF_DAY, MINUTE_OF_HOUR}
import java.util.concurrent.TimeUnit

import cats.effect.{Clock, ExitCode}
import monix.eval.{Task, TaskApp}

import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.language.implicitConversions
import scala.util.matching.Regex.Match

abstract class AdventApp(day: Int) extends TaskApp {

  def input: Vector[String] =
    Source.fromResource("day%02d.txt".format(day)).getLines().toVector

  private val now = Clock[Task].monotonic(TimeUnit.NANOSECONDS)

  override def run(args: List[String]): Task[ExitCode] =
    for {
      start <- now
      _ <- Task(go())
      end <- now
    } yield {
      val f = FiniteDuration(end - start, TimeUnit.NANOSECONDS)
      println(s"\nCompleted in ${f.toMillis} milliseconds")
      ExitCode.Success
    }

  def go(): Unit

  implicit def parseHelperString(symbol: Symbol)(implicit m: Match): String =
    m.group(symbol.name)

  implicit def parseHelperInt(symbol: Symbol)(implicit m: Match): Int =
    m.group(symbol.name).toInt

  implicit def parseHelperLong(symbol: Symbol)(implicit m: Match): Long =
    m.group(symbol.name).toLong

  private val timeOptionalSeconds = new DateTimeFormatterBuilder().appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':').appendValue(MINUTE_OF_HOUR, 2)
    .optionalStart
    .appendLiteral(':').appendValue(ChronoField.SECOND_OF_MINUTE, 2)
    .toFormatter

  private val customLocalDateTime = new DateTimeFormatterBuilder()
    .append(DateTimeFormatter.ISO_LOCAL_DATE)
    .optionalStart()
    .appendLiteral(' ')
    .append(timeOptionalSeconds)
    .toFormatter

  implicit class StringHelperMethods(s: String) {
    def toLocalDateTime: LocalDateTime =
      LocalDateTime.parse(s, customLocalDateTime)
  }

  implicit def parseHelperDateTime(symbol: Symbol)(implicit m: Match): LocalDateTime =
    m.group(symbol.name).toLocalDateTime

  def bound(min: Int, value: Int, max: Int): Int =
    Math.max(min, Math.min(value, max))

  implicit class StringTestInput(s: String) {
    def testInput: Vector[String] =
      s.stripMargin.lines.toVector
  }
}
