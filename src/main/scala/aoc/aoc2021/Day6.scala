package aoc.aoc2021

import aoc.Day

class Day6 extends Day(6, 2021) {

  val data = readData().getLines().toList(0).split(",").map(_.toInt).toList
  val fish: Map[Int, BigInt] = (0 to 8).map(k => (k, BigInt(data.count(_ == k)))).toMap

  def update(fish: Map[Int, BigInt], day: Int, endDay: Int): BigInt =
    if day == endDay then fish.values.sum
    else
      val newerMap: Map[Int, BigInt] = (0 to 8).toList.map { n =>
        n match {
          case 6 => 6 -> (fish(7) + fish(0))
          case 8 => 8 -> fish(0)
          case _ => n -> fish(n + 1)
        }
      }.toMap
      update(newerMap, day + 1, endDay)

  override def answer1(): Any = update(fish, 0, 80)

  override def answer2(): Any = update(fish, 0, 256)
}