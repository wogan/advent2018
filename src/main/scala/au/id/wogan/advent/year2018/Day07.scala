package au.id.wogan.advent.year2018

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day07 extends AdventApp(7) {

  val testInput: Vector[String] =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".testInput

  val regex: Regex = "Step (?<step>.+) must be finished before step (?<dep>.+) can begin.".r

  def parse(line: String): (String, String) = {
    implicit val m: Regex.Match = regex.findFirstMatchIn(line).get
    ('step, 'dep)
  }

  case class Step(name: String, prequisites: Set[String]) {
    def isAvailable(completed: Set[String]): Boolean =
      (prequisites -- completed).isEmpty

    def duration: Int =
      name.head - 'A' + 1
  }

  type Worker = Option[(String, Int)]

  override def go(): Unit = {
    val data = testInput map parse foldMap {
      case (step, dep) =>
        Map(step -> Set.empty[String], dep -> Set(step))
    } map (Step.apply _).tupled

    val (letter, _) = findOrder(List.empty, data.toSet)
    println(s"Part One: ${letter.mkString("")}")


  }

  def findOrderWithTime(time: Int, completed: Set[String], workers: List[Worker], remaining: Set[Step]): (Int, Set[String], List[Worker], Set[Step]) = {
    val available = remaining.filter(_.isAvailable(completed))
    val updatedWorkers = workers map {
      case Some((_, t)) if t <= time =>
        None
      case other =>
        other
    }
    ???
  }

  @tailrec
  def findOrder(completed: List[String], remaining: Set[Step]): (List[String], Set[Step]) = {
    val set = completed.toSet
    val toRemove = remaining.filter(_.isAvailable(set)).minBy(_.name)
    val rest = remaining - toRemove
    if (rest.isEmpty)
      (completed :+ toRemove.name) -> rest
    else
      findOrder(completed :+ toRemove.name, rest)
  }
}
