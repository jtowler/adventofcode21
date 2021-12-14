package aoc.aoc2021

import aoc.Day

class Day7 extends Day(7, 2021) {

  private val data = readData().getLines().toList.head.split(",").map(_.toInt).toList

  def answer(f: Int => Int): Int = (data.min to data.max).map { i =>
    data.map(x => f((x - i).abs)).sum
  }.min

  override def answer1(): Any = answer(identity)

  override def answer2(): Any = answer(a => (a * (a + 1)) / 2)
}