package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec
import scala.util.matching.Regex

class Day18 extends Day(18, 2021) {

  def add(s1: String, s2: String): String = s"[$s1,$s2]"

  def addAll(strings: List[String], s: String): String = strings match {
    case Nil => s
    case h :: t => addAll(t, add(s, h))
  }

  val singlePairPattern: Regex = """(\[\d+,\d+\])""".r
  val numbersFromPairPattern: Regex = """\[(\d+),(\d+)\]""".r
  val largeValuePattern: Regex = """(\d{2,})""".r
  val anyValuePattern: Regex = """(\d+)""".r

  def findExploder(s: String): Option[Regex.Match] =
    val pairs = singlePairPattern.findAllMatchIn(s)

    @tailrec
    def loop(): Option[Regex.Match] =
      if pairs.isEmpty then None
      else
        val next = pairs.next()
        val slice = s.slice(0, next.start)
        val brackCount = slice.count(_ == '[') - slice.count(_ == ']')
        if brackCount > 3 then
          Some(next)
        else
          loop()
    loop()

  def explode(s: String, r: Regex.Match): String =
    val (start, end) = (r.start, r.end)
    val exploder = s.slice(start, end)
    val numbersFromPairPattern(nl, nr) = exploder
    val numberToLeft = anyValuePattern.findAllMatchIn(s.slice(0, start)).toList.lastOption
    val numberToRight = anyValuePattern.findFirstMatchIn(s.slice(end, s.length))
    val iToLeftStart = if numberToLeft.nonEmpty then numberToLeft.get.start else 0
    val iToLeftEnd = if numberToLeft.nonEmpty then numberToLeft.get.end else 0
    val iToRightStart = if numberToRight.nonEmpty then end + numberToRight.get.start else s.length
    val iToRightEnd = if numberToRight.nonEmpty then end + numberToRight.get.end else s.length

    val leftBrackCount = s.slice(iToLeftStart, start).count(c => (c == '[') || (c == ']'))
    val rightBrackCount = s.slice(end, iToRightStart).count(c => (c == '[') || (c == ']'))

    val explodeToRight: Boolean = rightBrackCount < leftBrackCount

    val newBracket = if explodeToRight then
      s"0,${nr.toInt + numberToRight.get.toString().toInt}"
    else
      s"${nl.toInt + numberToLeft.get.toString().toInt},0"

    val beforeBracket = if explodeToRight then s.slice(0, start) else s.slice(0, iToLeftStart)
    val afterBracket = if explodeToRight then s.slice(iToRightEnd, s.length) else s.slice(end, s.length)

    if explodeToRight && numberToLeft.nonEmpty then
      val newVal = nl.toInt + numberToLeft.get.toString().toInt
      val newBefore = beforeBracket.slice(0, iToLeftStart)
      val newAfter = beforeBracket.slice(numberToLeft.get.end, beforeBracket.length)
      s"$newBefore$newVal$newAfter$newBracket$afterBracket"
    else if !explodeToRight && numberToRight.nonEmpty then
      val newVal = nr.toInt + numberToRight.get.toString().toInt
      val newBefore = s.slice(end, iToRightStart)
      val newAfter = s.slice(iToRightEnd, s.length)
      s"$beforeBracket$newBracket$newBefore$newVal$newAfter"
    else
      s"$beforeBracket$newBracket$afterBracket"

  def findSplitter(s: String): Option[Regex.Match] = largeValuePattern.findFirstMatchIn(s) match {
    case None => None
    case Some(p) => Some(p)
  }

  def split(s: String,r: Regex.Match): String =
    val (start, end) = (r.start, r.end)
    val s1 = s.slice(0, start)
    val s2 = s.slice(end, s.length)
    val n = s.slice(start, end).toInt
    s"$s1[${n / 2},${(n / 2) + 1}]$s2"


  def getMag(s: String): Int =

    def mag(st: String, r: Regex.Match) =
      val (start, end) = (r.start, r.end)
      val exploder = st.slice(start, end)
      val numbersFromPairPattern(a, b) = exploder
      a.toInt * 3 + b.toInt * 2

    @tailrec
    def loop(s: String): Int =
      if singlePairPattern.matches(s) then mag(s, singlePairPattern.findFirstMatchIn(s).get)
      else
        val mat = singlePairPattern.findFirstMatchIn(s).get
        val value = mag(s, mat)
        val before = s.slice(0, mat.start)
        val after = s.slice(mat.end, s.length)
        loop(s"$before$value$after")

    loop(s)

  private val data: List[String] = readData(true).getLines().toList

  override def answer1(): Any =

    @tailrec
    def loop(s: String): String =
      println(s)
      val exploder = findExploder(s)
      if exploder.nonEmpty then loop(explode(s, exploder.get))
      else
        val splitter = findSplitter(s)
        if splitter.nonEmpty then loop(split(s, splitter.get))
        else
          s

    val s = addAll(data.tail, data.head)
    println(s)
    val l = loop(s)
    println(l)
    getMag(l)

//    val s1 = "[[[[4,3],4],4],[7,[[8,4],9]]]"
//    val s2 = "[1,1]"
//    val s = add(s1, s2)
//    println(loop(s))
//    println("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

  override def answer2(): Any = ""
}