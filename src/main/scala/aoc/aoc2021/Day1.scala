package aoc.aoc2021

import aoc.Day

class Day1 extends Day(1, 2021) {

  private val depths = readData().getLines().map(_.toInt)

  override def answer1(): Any = depths.sliding(2).count(l=>l.head < l(1))

  override def answer2(): Any = depths.sliding(3).map(_.sum).sliding(2).count(l=>l.head < l(1))
}