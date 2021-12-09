package aoc.aoc2015

import aoc.Day

import java.security.MessageDigest
import scala.annotation.tailrec


class Day5 extends Day(5, 2015) {

  val data = readData().getLines().toList
  val vowels = List('a', 'e', 'i', 'o', 'u')
  val bad = List("ab", "cd", "pq", "xy")

  def countVowels(s: String): Boolean = s.toCharArray.count(vowels contains _) >= 3

  def noBad(s: String): Boolean = bad.find(s contains _) match {
    case Some(_) => false
    case None => true
  }

  def doubles(s: String): Boolean = s.sliding(2).find(_.distinct.length == 1) match {
    case Some(_) => true
    case None => false
  }

  def doublePair(s: String): Boolean = (for {
    i <- 0 until s.length - 1
    j <- i + 2 until s.length - 1
    s1 = s.slice(i, i + 2)
    s2 = s.slice(j, j + 2)
    if s1 == s2
  } yield true).nonEmpty

  def oneBetween(s: String): Boolean =
    (0 until s.length - 2).find(x => s(x) == s(x+2)) match {
      case Some(_) => true
      case None => false
    }

  override def answer1(): Any = data.count(s => countVowels(s) && noBad(s) && doubles(s))

  override def answer2(): Any = data.count(s => oneBetween(s) && doublePair(s))
}