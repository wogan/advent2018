package au.id.wogan.advent.year2018

object Day08 extends AdventApp(8) {

  private val testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split(" ").map(_.toInt)


  def parse(lines: List[Int]): (Node, List[Int]) =
    lines match {
      case 0 :: numM :: rest =>
        Node(Nil, rest.take(numM)) -> rest.drop(numM)
      case 1 :: numM :: rest =>
        val (child, suffix) = parse(rest)
        Node(List(child), suffix.take(numM)) -> suffix.drop(numM)
      case 2 :: numM :: rest =>
        val (child1, suffix) = parse(rest)
        val (child2, suffix2) = parse(suffix)
        Node(List(child1, child2), suffix2.take(numM)) -> suffix2.drop(numM)
    }

  def getNodes(int: Int, lines: List[Int]): (List[Node], List[Int]) = {
    if (int == 0) {
      Nil -> lines
    } else {
      ???
    }
  }

  case class Node(children: List[Node], metadata: List[Int])

  def sumMeta(node: Node): Int =
    node.metadata.sum + node.children.map(sumMeta).sum

  override def go(): Unit = {
    // Building a tree depth first
    val (tree, remainder) = parse(testInput.toList)
    println(s"Hack part one: ${sumMeta(tree)}")


  }
}
