package aoc.aoc2015

import aoc.Day

case class Light(ins: String, x1: Int, y1: Int, x2: Int, y2: Int) {

  def operateOne(in: Boolean): Boolean = ins match {
    case "turn on" => true
    case "turn off" => false
    case "toggle" => !in
  }

  def operate(lights: List[List[Boolean]]): List[List[Boolean]] = List.tabulate(lights.size, lights.head.size) {
    (x, y) =>
      val inRange = x >= x1 && x <= x2 && y >= y1 && y <= y2
      val current = lights(x)(y)
      if inRange then operateOne(current) else current
  }

}

class Day6 extends Day(6, 2015) {

  val pattern = """(.+) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)""".r
  val data = readData().getLines().map{ s =>
    val pattern(ins, x1, y1, x2, y2) = s
    Light(ins, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }.toList

  val lights = List.tabulate(1000, 1000)((_, _) => false)

  override def answer1(): Any =
    def loop(ins: List[Light], ls: List[List[Boolean]]): Int = ins match {
      case Nil => ls.flatten.count(_ == true)
      case h :: t => loop(t, h.operate(ls))
    }
    loop(data, lights)

  override def answer2(): Any = ""
}