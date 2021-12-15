package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

case class Say(count: Int, digit: Char)

class Day10 extends Day(10, 2015) {
  private val test = "1"
  private val actual = "3113322113"

  val actualList = mySay(test.toCharArray.toList, List())
  println(test)
  println(actualList.mkString)
  
  private def mySay(input: List[Char], output: List[Say]): List[Say] = input match {
    case Nil => output
    case h :: t =>
      val index: Int = t.indexWhere(_ != h)
      val extras = 1 + index
      val remaining = t.slice(index, t.size)
      val newOutput: List[Say] = Say(if index < 0 then 1 else index + 1, h) :: output
      mySay(remaining, newOutput).reverse
  }

  val target = 30

  @tailrec
  private def loopSay(i: Int, says: List[Say]): List[Say] = i match {
    case n if n == target => says
    case _ =>
//      val newOutput: List[Say] = Say(if index < 0 then 1 else index + 1, h) :: output
//      mySay(remaining, newOutput)
        loopSay(i +1, says)
  }

  def sayToString(says: List[Say]): String =
    says.reverse.map(x => s"${x.count}${x.digit}").mkString

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