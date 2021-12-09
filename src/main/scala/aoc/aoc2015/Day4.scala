package aoc.aoc2015

import aoc.Day

import java.security.MessageDigest
import scala.annotation.tailrec


class Day4 extends Day(4, 2015) {

  val key = "yzbqklnj"

  def getMd5(inputStr: String): String =
    val md: MessageDigest = MessageDigest.getInstance("MD5")
    md.digest(inputStr.getBytes()).map(0xFF & _).map {
      "%02x".format(_)
    }.foldLeft("") {
      _ + _
    }

  @tailrec
  private def loop(i: Int, pref: String): Int =
    val nKey = getMd5(key + i.toString)
    if nKey.startsWith(pref) then i
    else loop(i + 1, pref)

  override def answer1(): Any = loop(0, "00000")

  override def answer2(): Any = loop(0, "000000")
}