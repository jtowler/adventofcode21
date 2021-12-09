class Day8 extends Day(8) {

  private val data = readData().getLines().map(_.split(""" \| """)).toList
  private val easy = data.map(_ (1).split(" "))//.map(_.toCharArray.sorted.toList))

  override def answer1(): Any = easy.map(line => line.count(List(2, 3, 4, 7) contains _.length)).sum

  override def answer2(): Any =

    val myMap = Map(49 -> 8, 37 -> 5, 34 -> 2, 39 -> 3, 25 -> 7, 45 -> 9, 41 -> 6, 30 -> 4, 42 -> 0, 17 -> 1)

    def determineVals(inputs: Array[String], displays: Array[String]): Int =
      val charCounts = ('a' to 'g').map(char => char -> inputs.count(_ contains char)).toMap
      val displayVals = displays.map(_.map(c => charCounts(c)).sum)
      displayVals.map(myMap).mkString.toInt

    val hard = data.map(_.head.split(" "))//.map(_.toCharArray.sorted.toList))
    (hard zip easy).map(determineVals).sum
}