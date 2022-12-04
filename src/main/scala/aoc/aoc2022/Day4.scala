package aoc.aoc2022

import aoc.Day

import scala.annotation.tailrec

case class Pairs(min1: Int, max1: Int, min2: Int, max2: Int) {
  def contains: Boolean = {
    if min1 <= min2 && max1 >= max2 then true
    else if min2 <= min1 && max2 >= max1 then true
    else false
  }
  def partialContains: Boolean = ((min1 to max1) intersect (min2 to max2)).nonEmpty

}

class Day4 extends Day(4, 2022) {
  private val pattern = """(\d+)-(\d+),(\d+)-(\d+)""".r
  private val pairs = readData().getLines().map{ s =>
    val pattern(x1, y1, x2, y2) = s
    Pairs(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }.toList

  //  475
  override def answer1(): Any = pairs.count(_.contains)

  //  825
  override def answer2(): Any = pairs.count(_.partialContains)

}
