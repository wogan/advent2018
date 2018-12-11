package au.id.wogan.advent.year2018

object Day07 extends AdventApp(7) {

  val testInput: Vector[String] =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".testInput

  override def go(): Unit = {

  }
}
