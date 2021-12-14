package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

class Day10 extends Day(10, 2015) {
  private val test = "1"
  private val actual = "3113322113"

  @tailrec
  private def doSay(input: List[Char], output: String): String = input match {
    case Nil => output
    case h :: t =>
      val extras = 1 + t.takeWhile(_ == h).size
      val remaining = t.dropWhile(_ == h)
      val newString = output + s"$extras$h"
      doSay(remaining, newString)
  }

  @tailrec
  private def loopSay(s: String, i: Int, limit: Int): String = i match {
    case x if x == limit => s
    case _ => loopSay(doSay(s.toCharArray.toList, ""), i + 1, limit)
  }

  override def answer1(): Any = "" //loopSay(actual, 0, 40).length

  override def answer2(): Any = loopSay(actual, 0, 50).length

}