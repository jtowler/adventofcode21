import scala.io.Source

object main extends App {
  val dir = "resources"
  val day = 1
  val data = Source.fromFile(s"$dir/day_$day.txt")
  for (line <- data.getLines) {
    println(line)
  }
}