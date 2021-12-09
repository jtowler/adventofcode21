package aoc.aoc2021

import aoc.Day

case class Line (x1: Int, y1: Int, x2: Int, y2: Int) {

  def line(a: Int, b: Int): Range = a to b by (if (a > b) -1 else 1)

  def genLine(): List[(Int, Int)] = (x1, y1, x2, y2) match {
    case (x1, y1, x2, y2) if x1 == x2 => line(y1, y2).map((x1, _)).toList
    case (x1, y1, x2, y2) if y1 == y2 => line(x1, x2).map((_, y1)).toList
    case (x1, y1, x2, y2) => (line(x1, x2) zip line(y1, y2)).toList
  }

  def isHorVer(): Boolean = x1 == x2 || y1 == y2
}


class Day5 extends Day(5, 2021) {
  val pattern = """(\d+),(\d+) -> (\d+),(\d+)""".r
  val data = readData().getLines().map{ s =>
    val pattern(x1, y1, x2, y2) = s
    Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }.toList

  override def answer1(): Any =
    val lines = data.filter(_.isHorVer()).map(_.genLine()).flatten
    lines.groupBy(identity).map((k, v) => v.size).filter(_ > 1).size

  override def answer2(): Any =
    val lines = data.map(_.genLine()).flatten
    lines.groupBy(identity).map((k, v) => v.size).filter(_ > 1).size
}