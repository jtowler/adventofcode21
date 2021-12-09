package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

class Day1 extends Day(1, 2015) {

  val floors = readData().getLines().toList.head
  override def answer1(): Any = floors.count(_=='(') - floors.count(_==')')

  override def answer2(): Any =
    @tailrec
    def loop(i: Int, floor: Int, str: List[Char]): Int = (floor, str) match {
      case (-1, _) => i
      case (floor, '(' :: t) => loop(i + 1, floor + 1, t)
      case (floor, ')' :: t) => loop(i + 1, floor - 1, t)
      case _ => throw new RuntimeException
    }
    loop(0, 0, floors.toCharArray.toList)
}