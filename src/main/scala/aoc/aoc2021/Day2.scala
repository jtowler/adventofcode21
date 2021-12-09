package aoc.aoc2021

import aoc.Day

class Day2 extends Day(2, 2021) {

  val data = readData().getLines().map(_.split(' '))
    .map(x => (x(0), x(1).toInt)).toList


  override def answer1(): Any =
    def loop(ins: List[(String, Int)], h: Int, v: Int): Int = ins match
      case Nil => h * v
      case ("forward", n) :: tail => loop(tail, h + n, v)
      case ("down", n) :: tail => loop(tail, h, v + n)
      case ("up", n) :: tail => loop(tail, h, v - n)
      case _ => throw new RuntimeException

    loop(data, 0, 0)

  override def answer2(): Any =
    def loop(ins: List[(String, Int)], h: Int, v: Int, a: Int): Int = ins match
      case Nil => h * v
      case ("forward", n) :: tail => loop(tail, h + n, v + n * a, a)
      case ("down", n) :: tail => loop(tail, h, v, a + n)
      case ("up", n) :: tail => loop(tail, h, v, a - n)
      case _ => throw new RuntimeException

    loop(data, 0, 0, 0)
}