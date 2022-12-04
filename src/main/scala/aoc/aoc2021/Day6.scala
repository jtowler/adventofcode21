package aoc.aoc2021

import aoc.Day

class Day6 extends Day(6, 2021) {

  private val data: List[Int] = readData().getLines().toList.head.split(",").map(_.toInt).toList
  private val fish: Map[Int, BigInt] = (0 to 8).map(k => (k, BigInt(data.count(_ == k)))).toMap

  def update(day: Int): BigInt = (0 until day).foldLeft(fish)((f, _) => (0 to 8).toList.map {
    case 6 => 6 -> (f(7) + f(0))
    case 8 => 8 -> f(0)
    case n => n -> f(n + 1)
  }.toMap).values.sum

  override def answer1(): Any = update(80)

  override def answer2(): Any = update(256)
}