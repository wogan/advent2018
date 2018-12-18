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

  case class Step(name: String, prerequisites: Set[String]) {
    def isAvailable(completed: Set[String]): Boolean =
      (prerequisites -- completed).isEmpty

    def duration: Int =
      name.head - 'A' + 61
  }

  type Worker = Option[(String, Int)]

  override def go(): Unit = {
    val data = input map parse foldMap {
      case (step, dep) =>
        Map(step -> Set.empty[String], dep -> Set(step))
    } map (Step.apply _).tupled

    val (letter, _) = findOrder(List.empty, data.toSet)
    println(s"Part One: ${letter.mkString("")}")
    val state = findOrderWithTime(State(data.toSet))
    println(s"Part Two: ${state.time}")
  }

  case class State(workers: List[Worker], remaining: Set[Step], time: Int, completed: Set[String])
  object State {
    def apply(remaining: Set[Step], workerCount: Int = 5): State =
      new State(List.fill(workerCount)(None), remaining, 0, Set.empty)
  }

  @tailrec
  def findOrderWithTime(input: State): State = {
    import input._
    val updatedWorkers = workers.updateFor(time)
    val newCompleted = completed ++ workers.completed(time)
    if (updatedWorkers.idle && remaining.isEmpty) {
      input.copy(workers = updatedWorkers, completed = newCompleted)      // we're done!
    } else {
      val availableSteps = remaining.filter(_.isAvailable(newCompleted))
      val (newWorkers, takenWork) = updatedWorkers.perform(availableSteps, time)
      val newState = input.copy(
        completed = newCompleted,
        workers = newWorkers,
        time = newWorkers.nextTime,
        remaining = remaining -- takenWork)
      findOrderWithTime(newState)
    }
  }

  implicit class WorkerMethods(workers: List[Worker]) {
    def updateFor(time: Int): List[Worker] =
      workers map {
        case Some((_, t)) if t <= time =>
          None
        case other =>
          other
      }
    def completed(time: Int): List[String] =
      workers collect {
        case Some((name, t)) if t <= time =>
          name
      }
    def idle: Boolean =
      workers.forall(_.isEmpty)
    def perform(steps: Set[Step], time: Int): (List[Worker], Set[Step]) = {
      val ordered = steps.toList.sortBy(_.name)
      val (working, idle) = workers.partition(_.isDefined)
      val s = Math.min(idle.size, ordered.size)
      val newWork = ordered take s
      val newWorkers = newWork map { step =>
        Some(step.name -> (time + step.duration))
      }
      working ++ newWorkers ++ List.fill(workers.size - s - working.size)(None) -> newWork.toSet
    }
    def nextTime: Int =
      workers.collect {
        case Some((_, t)) => t
      }.min
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
