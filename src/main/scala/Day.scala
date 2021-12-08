import scala.io.{BufferedSource, Source}

trait Day(day: Int) {

  def answer1(): Any

  def answer2(): Any

  def display(): Unit =
    println(answer1())
    print(answer2())

  def readData(test: Boolean = false, bigboy: Boolean = false): BufferedSource =
    val suffix = (test, bigboy) match {
      case (_, true) => "_bigboy"
      case (true, _) => "_test"
      case _ => ""
    }
    Source.fromFile(s"resources/day_$day$suffix.txt")
}