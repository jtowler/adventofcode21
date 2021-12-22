package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec


class Day20 extends Day(20, 2021) {

  val data: List[String] = readData().getLines().toList

  val algo: List[Int] = data.head.map(v => if v == '#' then 1 else 0).toList
  val inputImage: List[List[Int]] = data.slice(2, data.size).map(_.map(v => if v == '#' then 1 else 0).toList)

  override def answer1(): Any = ""

  override def answer2(): Any = ""
}