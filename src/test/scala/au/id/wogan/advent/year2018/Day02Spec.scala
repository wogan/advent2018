package au.id.wogan.advent.year2018

import cats.data.Ior

object Day02Spec extends App {

  implicit class StringHelper(s: String) {
    def counts(result: Option[Ior[Long, Long]]): Unit = {
      if (Day02.countFor(s) != result)
        println(s"Error: '$s' did not result in $result")
    }

    def matches(other: String): Unit = {
      if (!Day02.compare(s, other)) {
        println(s"Error: '$s' did not match $other")
      }
    }
    def notMatches(other: String): Unit = {
      if (Day02.compare(s, other)) {
        println(s"Error: '$s' did match $other")
      }
    }
  }

  implicit class ListStringHelper(s: Seq[String]) {
    def totals(result: Option[Ior[Long, Long]]): Unit = {
      val res = s.toList foldMap Day02.countFor
      if (res != result)
          println(s"Error: '$res' did not total $result")
    }

    def sumsTo(long: Long): Unit = {
      val res = Day02.checksum(s.toVector)
      if (res != long)
        println(s"Error: '$res' did not equal $long")
    }

    def finds(one: String, two: String): Unit = {
      val res = Day02.findPair(s.toList).get
      if (res != (one, two) && res != (two, one)) {
        println(s"Error: $res was not $one, $two")
      }
    }
  }

  "abcdef" counts None
  "bababc" counts Ior.both(1L, 1L).some
  "abbcde" counts 1L.leftIor[Long].some
  "abcccd" counts 1L.rightIor[Long].some
  "aabcdd" counts 1L.leftIor[Long].some
  "abcdee" counts 1L.leftIor[Long].some
  "ababab" counts 1L.rightIor[Long].some

  Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab") totals Some(Ior.Both(4L, 3L))
  Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab") sumsTo 12L

  "fghij" matches "fguij"
  "abcde" notMatches "axcye"

  Seq("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz") finds ("fghij", "fguij")
}
