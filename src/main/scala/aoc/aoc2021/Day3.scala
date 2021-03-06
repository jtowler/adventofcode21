package aoc.aoc2021

import aoc.Day

class Day3 extends Day(3, 2021) {

  val data = readData().getLines().toList
  val numEntries = data.size
  val numLength = data(0).length

  override def answer1(): Any =
    val s = List.range(0, numLength).map({ pos =>
      if data.count(_ (pos) == '1') > numEntries / 2 then 1 else 0
    })
    val s2 = s.map {
      _ match
        case 1 => 0
        case 0 => 1
    }
    Integer.parseInt(s.mkString, 2) * Integer.parseInt(s2.mkString, 2)

  override def answer2(): Any =
    def loop(data: List[String], pos: Int, func: (Int, Int) => Boolean): Int = {
      if data.size == 1 then
        Integer.parseInt(data(0).mkString, 2)
      else {
        val ones = data.count(_ (pos) == '1')
        val zeros = data.count(_ (pos) == '0')
        val mostCommon = if func(ones, zeros) then '1' else '0'
        loop(data.filter(_ (pos) == mostCommon), pos + 1, func)
      }
    }

    loop(data, 0, _ >= _) * loop(data, 0, _ < _)
}