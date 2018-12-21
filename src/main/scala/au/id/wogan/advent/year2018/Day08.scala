package au.id.wogan.advent.year2018

object Day08 extends AdventApp(8) {

  def getNodes(s: Int, data: List[Int]): (List[Node], List[Int]) = {
    val c :: m :: rest = data
    val (node, remaining) = if (c == 0) {
      Node(Nil, rest take m) -> rest.drop(m)
    } else {
      val (children, post) = getNodes(c - 1, rest)
      Node(children, post take m) -> post.drop(m)
    }
    if (s == 0) {
      List(node) -> remaining
    } else {
      val (siblings, afterSiblings) = getNodes(s - 1, remaining)
      (node +: siblings) -> afterSiblings
    }
  }

  case class Node(children: List[Node], metadata: List[Int]) {
    def totalMeta: Int =
      metadata.sum + children.map(_.totalMeta).sum

    def value: Int =
      if (children.isEmpty)
        metadata.sum
      else {
        val selected = metadata filter (_ > 0) map (_ - 1) flatMap (children.get(_).toList)
        selected.map(_.value).sum
      }
  }

  override def go(): Unit = {
    val data = input.head.split(" ").map(_.toInt).toList
    val (List(tree), remainder) = getNodes(0, data)
    assert(remainder.isEmpty)
    println(s"Part One: ${tree.totalMeta}")
    println(s"Part Two: ${tree.value}")
  }
}
