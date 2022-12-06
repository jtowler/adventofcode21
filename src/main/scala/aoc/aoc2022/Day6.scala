package aoc.aoc2022

import aoc.Day

class Day6 extends Day(6, 2022) {

  private val raw = readData().getLines().next()

  def sliding(x: Int) = x + raw.sliding(x).indexWhere(_.distinct.length == x)

  //  1093
  override def answer1(): Any = sliding(4)

  //  3534
  override def answer2(): Any = sliding(14)

}
