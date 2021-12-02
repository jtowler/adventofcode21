import scala.io.Source

trait Day(day: Int) {

  def answer1(): Any

  def answer2(): Any

  def display() =
    println(answer1())
    print(answer2())

  def readData(test: Boolean = false) =
    val prefix = if test then "_test" else ""
    Source.fromFile(s"resources/day_$day$prefix.txt")
}