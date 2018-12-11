package au.id.wogan.advent.year2018

import java.time.LocalDateTime

import scala.util.matching.Regex

object Day04 extends AdventApp(4) {

  val testInput: String = """[1518-11-01 00:00] Guard #10 begins shift
                    |[1518-11-01 00:05] falls asleep
                    |[1518-11-01 00:25] wakes up
                    |[1518-11-01 00:30] falls asleep
                    |[1518-11-01 00:55] wakes up
                    |[1518-11-01 23:58] Guard #99 begins shift
                    |[1518-11-02 00:40] falls asleep
                    |[1518-11-02 00:50] wakes up
                    |[1518-11-03 00:05] Guard #10 begins shift
                    |[1518-11-03 00:24] falls asleep
                    |[1518-11-03 00:29] wakes up
                    |[1518-11-04 00:02] Guard #99 begins shift
                    |[1518-11-04 00:36] falls asleep
                    |[1518-11-04 00:46] wakes up
                    |[1518-11-05 00:03] Guard #99 begins shift
                    |[1518-11-05 00:45] falls asleep
                    |[1518-11-05 00:55] wakes up""" stripMargin '|'

  val regex: Regex = "\\[(?<datetime>.*)\\] (?<event>.*)".r
  val guard: Regex = "Guard #(?<id>\\d+) begins shift".r

  type Log = (LocalDateTime, String)

  def parseLine(line: String): Log = {
    implicit val m: Regex.Match = regex.findFirstMatchIn(line).get
    ('datetime: LocalDateTime) -> 'event
  }

  implicit val orderLocalDateTime: Ordering[LocalDateTime] = _ compareTo _

  def parse(list: List[String]): List[Guard] = {
    def start(head: Log, tail: Seq[Log]): List[Guard] = {
      implicit val m: Regex.Match = guard.findFirstMatchIn(head._2).get
      sleepsFor('id, Nil, tail)
    }
    def sleepsFor(gid: Int, sleeps: List[Sleep], events: Seq[Log]): List[Guard] = {
      events.headOption match {
        case Some((timestamp, "falls asleep")) =>
          awakes(gid, timestamp.getMinute, sleeps, events.tail)
        case Some(other) =>
          Guard(gid, sleeps) +: start(other, events.tail)
        case None =>
          List(Guard(gid, sleeps))
      }
    }
    def awakes(gid: Int, start: Int, sleeps: List[Sleep], events: Seq[Log]): List[Guard] =
      events.head match {
        case (timestamp, "wakes up") =>
          sleepsFor(gid, Sleep(start, timestamp.getMinute) +: sleeps, events.tail)
        case other =>
          throw new IllegalStateException(s"guard $gid never woke; instead $other")
      }
    val logs = list map parseLine sortBy (_._1)
    start(logs.head, logs.tail).groupBy(_.id).values.map(_.reduce((a, b) => Guard(a.id, a.sleeps ++ b.sleeps))).toList
  }

  case class Sleep(start: Int, end: Int) {
    def total: Int = end - start
    def minutes: Range = start until end
  }

  case class Guard(id: Int, sleeps: List[Sleep]) {
    val total: Int = sleeps.map(_.total).sum
    val byMinute: Map[Int, Int] = sleeps flatMap (_.minutes.toList) groupBy identity mapValues(_.size)
    val mostCommonAsleep: Option[(Int, Int)] = if (byMinute.nonEmpty) byMinute.maxBy(_._2).some else None
  }

  override def go(): Unit = {
    val data = parse(input.toList)
    val sleepiest = data.maxBy(_.total)
    val sleepiestMinute = sleepiest.mostCommonAsleep.map(_._1).get
    println(s"Part One: Guard #${sleepiest.id} most commonly asleep on minute $sleepiestMinute: ${sleepiest.id * sleepiestMinute}")
    val consistent = data.maxBy(_.mostCommonAsleep map (_._2))
    val consistentMinute = consistent.mostCommonAsleep.map(_._1).get
    println(s"Part Two: Guard #${consistent.id} most commonly asleep on minute $consistentMinute: ${consistent.id * consistentMinute}")
  }

}
