package aoc.aoc2022

import aoc.Day

import scala.annotation.tailrec

class Day1 extends Day(1, 2022) {

  private val raw = readData().mkString("").split("\n\n").toList
  val sums: List[Int] = raw.map(_.linesIterator.map(_.toInt).sum)

//  71124
  override def answer1(): Any = sums.max

//  204639
  override def answer2(): Any = sums.sortBy(-_).take(3).sum
}
