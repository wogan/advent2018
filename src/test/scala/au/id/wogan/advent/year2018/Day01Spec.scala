package au.id.wogan.advent.year2018

object Day01Spec extends App {

  implicit class Helper(s: String) {
    val parsed: Seq[Long] = s.split(", ").map(_.toLong)

    def resultsIn(long: Long): Unit = {
      if (parsed.sum != long)
        println(s"Error: '$s' did not result in $long")
    }

    def firstReaches(long: Long): Unit = {
      if (Day01.firstRepeatedFrequency(parsed) != long)
        println(s"Error: '$s' did not reach $long")
    }
  }

  "+1, +1, +1" resultsIn 3
  "+1, +1, -2" resultsIn 0
  "-1, -2, -3" resultsIn -6

  "+1, -1" firstReaches 0
  "+3, +3, +4, -2, -4" firstReaches 10
  "-6, +3, +8, +5, -6" firstReaches 5
  "+7, +7, -2, -7, -4" firstReaches 14
}
