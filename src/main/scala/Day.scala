import scala.io.Source

trait Day(day: Int) {

  def answer1(): Any

  def answer2(): Any

  def display() =
    println(answer1())
    print(answer2())

  def readData() =
    Source.fromFile(s"resources/day_$day.txt")
}