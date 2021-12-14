package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

class Day8 extends Day(8, 2015) {
  private val data = readData().getLines().toList
  private val uniPattern = """(\\x[0-9a-f]{2})""".r

  def matchUni(s: String): Option[String] = s match {
    case uniPattern(x) => Some(x)
    case _ => None
  }

  def countChar(string: String, total: Int): Int = string match {
    case s if s.isEmpty => total
    case s if (s.length > 1) && (s(0) == '\\') && (s(1) == '\"') =>
      countChar(s.slice(2, s.length), total + 1)
    case s if (s.length > 1) && (s(0) == '\\') && (s(1) == '\\') =>
      countChar(s.slice(2, s.length), total + 1)
    case s if matchUni(s.slice(0, 4)).nonEmpty => countChar(s.slice(4, s.length), total + 1)
    case s => countChar(s.slice(1, s.length), total + 1)
  }

  def expandChar(string: List[Char], total: Int): Int = string match {
    case Nil => total + 2
    case '\\' :: t => expandChar(t, total + 2)
    case '\"' :: t => expandChar(t, total + 2)
    case _ :: t => expandChar(t, total + 1)

  }

  override def answer1(): Any =
    data.map(_.length).sum - data.map(x => countChar(x.slice(1, x.length - 1), 0)).sum

  override def answer2(): Any =
    data.map(x => expandChar(x.toCharArray.toList, 0)).sum - data.map(_.length).sum
}