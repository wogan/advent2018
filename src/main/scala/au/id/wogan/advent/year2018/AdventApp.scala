package au.id.wogan.advent.year2018

import java.time.LocalDateTime
import java.time.chrono.IsoChronology
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, ResolverStyle}
import java.time.temporal.ChronoField.{HOUR_OF_DAY, MINUTE_OF_HOUR}
import java.time.temporal.{ChronoField, ChronoUnit}

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

import scala.io.Source
import scala.util.matching.Regex.Match

abstract class AdventApp(day: Int) extends TaskApp {

  def input: Vector[String] =
    Source.fromResource("day%02d.txt".format(day)).getLines().toVector

  override def run(args: List[String]): Task[ExitCode] = Task {
    go()
    ExitCode.Success
  }

  def go(): Unit

  implicit def parseHelperString(symbol: Symbol)(implicit m: Match): String =
    m.group(symbol.name)
  implicit def parseHelperInt(symbol: Symbol)(implicit m: Match): Int =
    m.group(symbol.name).toInt
  implicit def parseHelperLong(symbol: Symbol)(implicit m: Match): Long =
    m.group(symbol.name).toLong

  private val timeNoMinutes = new DateTimeFormatterBuilder().appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':').appendValue(MINUTE_OF_HOUR, 2)
    .optionalStart
    .appendLiteral(':').appendValue(ChronoField.SECOND_OF_MINUTE, 2)
    .toFormatter

  private val formatter = new DateTimeFormatterBuilder()
    .append(DateTimeFormatter.ISO_LOCAL_DATE)
    .optionalStart()
    .appendLiteral(' ')
    .append(timeNoMinutes)
    .toFormatter

  implicit class StringHelperMethods(s: String) {
    def toLocalDateTime: LocalDateTime =
      LocalDateTime.parse(s, formatter)
  }

  implicit def parseHelperDateTime(symbol: Symbol)(implicit m: Match): LocalDateTime =
    m.group(symbol.name).toLocalDateTime
}
