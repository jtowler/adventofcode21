package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec


class Day3 extends Day(3, 2015) {

  @tailrec
  private def loop(inst: List[Char], visited: List[(Int, Int)], x: Int, y: Int): List[(Int, Int)] = inst match {
    case Nil => visited.distinct
    case '<' :: tail => loop(tail, (x, y) :: visited, x - 1, y)
    case '>' :: tail => loop(tail, (x, y) :: visited, x + 1, y)
    case 'v' :: tail => loop(tail, (x, y) :: visited, x, y - 1)
    case '^' :: tail => loop(tail, (x, y) :: visited, x, y + 1)
  }

  val data = readData().getLines().toList.head.toCharArray.toList

  override def answer1(): Any = loop(data, List((0, 0)), 0, 0).size

  override def answer2(): Any =
    val grouped = data.grouped(2).toList.transpose
    val santa = loop(grouped.head, List((0, 0)), 0, 0)
    val robot = loop(grouped.last, List((0, 0)), 0, 0)
    (santa ++ robot).distinct.size
}